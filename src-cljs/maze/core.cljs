(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [location {:keys [visited] :or {visited #{}}}]
  (->>
    (neighbors location)
    (remove visited)
    (set)))

(defn- blocked-by-wall? [location walls neighbor]
  (walls #{location neighbor}))

(defn- reachable-neighbors [location {:keys [visited walls]
                                      :or {walls #{} visited #{}}}]
  (set
    (remove (partial blocked-by-wall? location walls)
            (unvisited-neighbors location {:visited visited}))))

(defn- all-walls-for-location [location]
  (map
    (partial conj #{} location)
    (unvisited-neighbors location {})))

(defn- all-walls [{:keys [walls doors]
                   :or {doors #{}}}]
  (let [size (apply max (flatten (map seq walls)))
        locations (for [x (range size) y (range size)] [x y])]
    (difference
      (reduce into #{} (map all-walls-for-location locations))
      doors)))

(defn- random-reachable-neighbor [location {:keys [visited walls]
                                            :or {walls #{}}}]
  (rand-nth (seq (reachable-neighbors location {:visited visited
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
      (if (finished-fn current-location maze)
        (do
          (when update-channel (go (>! update-channel :finished)))
          (merge
            maze {:walls (all-walls {:walls walls :doors doors})}))
        (let [maze (merge maze {:visited (conj visited current-location)})]
          (if-let [next-location (next-location-fn current-location maze)]
            (search-maze (merge maze {:path (conj path next-location)
                                      :doors (conj doors #{current-location next-location})}))
            (search-maze (merge maze {:path (pop path)}))))))))

(defn- outer-walls [size]
  (set (flatten (concat (for [x (range size) y (range size)]
                          [#{[0 y] [-1 y]} #{[(dec size) y] [size y]}
                           #{[x 0] [x -1]} #{[x (dec size)] [x size]}])))))

(defn- all-locations-visited? [size location {:keys [visited]}]
    (= (count visited) (* size size)))

(defn- solved? [size location {}]
  (= location [(dec size) (dec size)]))

(defn generate-maze [maze]
  (search-maze (merge maze {:walls (outer-walls (:size maze))
                            :finished-fn (partial all-locations-visited? (:size maze))})))

(defn solve-maze [maze]
  (search-maze (merge maze {:finished-fn (partial solved? (:size maze))})))
