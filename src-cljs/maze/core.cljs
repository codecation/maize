(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [location {:keys [visited]
                                      :or {visited #{}}}]
    (->>
      (neighbors location)
      (remove visited)
      (set)))

(defn- blocked-by-wall? [current-location walls neighbor]
  (walls #{current-location neighbor}))

(defn- reachable-neighbors [location {:keys [visited walls]
                                     :or {walls #{} visited #{}}}]
  (let [within-maze-and-unvisited (unvisited-neighbors location {:visited visited})]
    (set
      (remove (partial blocked-by-wall? location walls)
              within-maze-and-unvisited))))

(defn- all-locations [size]
  (for [x (range size) y (range size)] [x y]))

(defn- all-walls-for-location [location]
  (map
    (partial conj #{} location)
    (unvisited-neighbors location {})))

(defn- all-walls [size]
  (reduce into #{} (map (partial all-walls-for-location)
                        (all-locations size))))

(defn- all-walls-on-perimeter [size]
  (set
    (flatten
      (concat
        (for [y (range size)]
          [#{[0 y] [-1 y]} #{[(dec size) y] [size y]}])
        (for [x (range size)]
          [#{[x 0] [x -1]} #{[x (dec size)] [x size]}])))))

(defn- all-walls-without-doors [{:keys [size doors]
                                 :or {doors #{}}}]
  (difference (all-walls size) doors))

(defn- all-locations-visited? [location {:keys [visited size]
                                         :or {visited #{}}}]
  (= (count visited) (* size size)))

(defn- solved? [location {:keys [size]}]
  (= location [(dec size) (dec size)]))

(defn- random-reachable-neighbor [location {:keys [visited walls size]
                                            :or {walls #{}}}]
  (rand-nth (seq (reachable-neighbors location {:visited visited
                                                :walls walls}))))

(defn search-maze [{:keys [path visited walls doors size update-channel next-location-fn finished-fn]
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
            maze {:walls (all-walls-without-doors {:size size :doors doors})}))
        (let [maze (merge maze {:visited (conj visited current-location)})]
          (if-let [next-location (next-location-fn current-location maze)]
            (search-maze (merge maze {:path (conj path next-location)
                                      :doors (conj doors #{current-location next-location})}))
            (search-maze (merge maze {:path (pop path)}))))))))

(defn generate-maze [maze]
  (search-maze (merge maze {:walls (all-walls-on-perimeter (:size maze))
                            :finished-fn all-locations-visited?})))

(defn solve-maze [maze]
  (search-maze (merge maze {:finished-fn solved?})))
