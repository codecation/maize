(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [{:keys [location visited]}]
  (->>
    (neighbors location)
    (remove visited)
    (set)))

(defn- blocked-by-wall? [location neighbor walls]
  (walls #{location neighbor}))

(defn- reachable-neighbors [{:keys [location visited walls]}]
  (set
    (remove #(blocked-by-wall? location % walls)
            (unvisited-neighbors {:location location :visited visited}))))

(defn- walls-around-location [location]
  (map
    (partial conj #{} location)
    (unvisited-neighbors {:location location :visited {}})))

(defn- add-inner-walls [{:keys [outer-walls doors]}]
  (let [size (apply max (flatten (map seq outer-walls)))
        locations (for [x (range size) y (range size)] [x y])
        all-walls (reduce into #{} (map walls-around-location locations))]
    (difference all-walls doors)))

(defn- random-reachable-neighbor [{:keys [location visited walls]}]
  (rand-nth (seq (reachable-neighbors {:location location
                                       :visited visited
                                       :walls walls}))))

(defn search-maze [{:keys [path visited walls doors update-channel next-location-fn finished-fn]
                    :or {next-location-fn random-reachable-neighbor
                         path [[0 0]]
                         visited #{}
                         doors #{}}
                    :as maze}]
  (let [current-location (peek path)]
    (do
      (when update-channel (go (>! update-channel maze)))
      (if (finished-fn (merge maze {:path path}))
        (do
          (when update-channel (go (>! update-channel :finished)))
          (merge
            maze {:walls (add-inner-walls {:outer-walls walls :doors doors})}))
        (let [maze (merge maze {:visited (conj visited current-location)})]
          (if-let [next-location (next-location-fn
                                   (merge maze {:location current-location}))]
            (search-maze (merge maze {:path (conj path next-location)
                                      :doors (conj doors #{current-location next-location})}))
            (search-maze (merge maze {:path (pop path)}))))))))

(defn- all-locations-visited? [{:keys [path]}]
  (empty? path))

(defn- solved? [{:keys [size path]}]
  (= (peek path) [(dec size) (dec size)]))

(defn- outer-walls [{:keys [size]}]
  (set (flatten (concat (for [x (range size) y (range size)]
                          [#{[0 y] [-1 y]} #{[(dec size) y] [size y]}
                           #{[x 0] [x -1]} #{[x (dec size)] [x size]}])))))

(defn generate-maze [maze]
  (search-maze (merge maze {:walls (outer-walls maze)
                            :finished-fn all-locations-visited?})))

(defn solve-maze [maze]
  (search-maze (merge maze {:finished-fn solved?})))
