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

(defn- add-inner-walls [{:keys [walls doors size]}]
  (let [locations (for [x (range size) y (range size)] [x y])
        all-walls (reduce into #{} (map walls-around-location locations))]
    (difference all-walls doors)))

(defn- next-paths [{:keys [current-path] :as maze}]
  (->>
    (reachable-neighbors maze)
    (map #(conj current-path %))
    vec))

(defn- shuffled-next-paths [maze]
  (shuffle (next-paths maze)))

(defn- depth-first [paths]
  [(peek paths) (pop paths)])

(defn- breadth-first [paths]
  [(first paths) (rest paths)])

(defn search-maze [{:keys [paths visited walls doors update-channel
                           search-algorithm next-paths-fn finished-fn]
                    :or {search-algorithm depth-first
                         next-paths-fn shuffled-next-paths
                         paths [[[0 0]]] visited #{} doors #{}}
                    :as maze}]
  (let [[current-path paths] (search-algorithm paths)
        current-location (peek current-path)]
    (if (visited current-location)
      (search-maze (merge maze {:paths paths}))
      (let [previous-location (peek (pop current-path))
            visited (conj visited current-location)
            doors (conj doors #{previous-location current-location})
            maze (merge maze {:current-path current-path
                              :visited visited
                              :doors doors})]
        (when update-channel (go (>! update-channel maze)))
        (if (finished-fn maze)
          (do
            (when update-channel (go (>! update-channel :finished)))
            (merge maze {:walls (add-inner-walls maze)}))
          (let [next-paths (vec (concat paths (next-paths-fn maze)))]
            (search-maze (merge maze {:paths next-paths}))))))))

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
