(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [{:keys [current-path visited]}]
  (let [current-location (peek current-path)]
    (->>
      (neighbors current-location)
      (remove visited)
      (set))))

(defn- blocked-by-wall? [location neighbor walls]
  (walls #{location neighbor}))

(defn- reachable-neighbors [{:keys [current-path visited walls] :as maze}]
  (let [current-location (peek current-path)]
    (set
      (remove #(blocked-by-wall? current-location % walls)
              (unvisited-neighbors maze)))))

(defn- walls-around-location [location]
  (map
    (partial conj #{} location)
    (neighbors location)))

(defn- add-inner-walls [{:keys [walls doors]}]
  (let [size (apply max (flatten (map seq walls)))
        locations (for [x (range size) y (range size)] [x y])
        all-walls (reduce into #{} (map walls-around-location locations))]
    (difference all-walls doors)))

(defn- next-paths [{:keys [current-path] :as maze}]
  (->>
    (reachable-neighbors maze)
    (map #(conj current-path %))
    vec))

(defn- shuffled-next-paths [maze]
  (shuffle (next-paths maze)))

(defn search-maze [{:keys [path visited walls doors update-channel next-location-fn finished-fn]
                    :or {next-location-fn random-reachable-neighbor
                         path [[0 0]]
                         visited #{}
                         doors #{}}
                    :as maze}]
  (let [current-location (peek path)]
    (when update-channel (go (>! update-channel maze)))
    (if (finished-fn (merge maze {:location current-location}))
      (do
        (when update-channel (go (>! update-channel :finished)))
        (merge
          maze {:walls (add-inner-walls {:outer-walls walls :doors doors})}))
      (let [maze (merge maze {:visited (conj visited current-location)})]
        (if-let [next-location (next-location-fn
                                 (merge maze {:location current-location}))]
          (search-maze (merge maze {:path (conj path next-location)
                                    :doors (conj doors #{current-location next-location})}))
          (search-maze (merge maze {:path (pop path)})))))))

(defn- all-locations-visited? [{:keys [visited size]}]
  (= (count visited) (* size size)))

(defn- solved? [{:keys [current-path size]}]
  (let [current-location (peek current-path)]
    (= current-location [(dec size) (dec size)])))

(defn- outer-walls [{:keys [size]}]
  (set (flatten (concat (for [x (range size) y (range size)]
                          [#{[0 y] [-1 y]} #{[(dec size) y] [size y]}
                           #{[x 0] [x -1]} #{[x (dec size)] [x size]}])))))

(defn generate-maze [maze]
  (search-maze (merge maze {:walls (outer-walls maze)
                            :finished-fn all-locations-visited?})))

(defn solve-maze [maze]
  (search-maze (merge maze {:finished-fn solved?})))
