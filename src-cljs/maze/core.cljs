(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [location visited size]
  (letfn [(outside-bounds? [[x y]]
            ((some-fn neg? #(> % (dec size))) x y))]
    (->>
      (neighbors location)
      (remove outside-bounds?)
      (remove visited)
      (set))))

(defn- blocked-by-wall? [current-location walls neighbor]
  (walls #{current-location neighbor}))

(defn- reachable-neighbors [location {:keys [visited walls size]}]
  (let [within-maze-and-unvisited (unvisited-neighbors location visited size)]
    (set
      (remove (partial blocked-by-wall? location walls)
              within-maze-and-unvisited))))

(defn- random-unvisited-neighbor [location {:keys [visited size]}]
  (rand-nth (seq (unvisited-neighbors location visited size))))

(defn- walls-without-doors [walls doors]
  (difference walls doors))

(defn- all-locations [size]
  (for [x (range size) y (range size)] [x y]))

(defn- all-walls [size location]
  (map (partial conj #{} location) (unvisited-neighbors location #{} size)))

(defn- fully-walled-grid [size]
  (reduce into #{} (map (partial all-walls size) (all-locations size))))

(defn- solved? [location {:keys [size]}]
  (= location [(dec size) (dec size)]))

(defn- random-reachable-neighbor [location {:keys [visited walls size]}]
  (rand-nth
    (seq
      (reachable-neighbors location {:visited visited
                                     :walls walls
                                     :size size}))))

(defn search-maze [{:keys [path visited walls doors size update-channel next-location-fn finished-fn]
                    :as maze}]
  (if-let [current-location (peek path)]
    (do
      (when update-channel (go (>! update-channel maze)))
      (if (finished-fn current-location maze)
        (do
          (when update-channel (go (>! update-channel :solved)))
          maze)
        (let [maze (merge maze {:visited (conj visited current-location)})]
          (if-let [next-location (next-location-fn current-location maze)]
            (search-maze (merge
                           maze {:path (conj path next-location)
                                 :doors (conj doors #{current-location next-location})}))
            (search-maze (merge
                           maze {:path (pop path)}))))))
    (merge
      maze {:walls (walls-without-doors (fully-walled-grid size) doors)})))

(defn generate-maze [maze]
  (search-maze (merge maze {:next-location-fn random-unvisited-neighbor
                            :finished-fn (fn [_ _])})))

(defn solve-maze [maze]
  (search-maze (merge maze {:next-location-fn random-reachable-neighbor
                            :finished-fn solved?})))
