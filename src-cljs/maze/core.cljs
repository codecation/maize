(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(declare maze-size)

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [location visited]
  (letfn [(outside-bounds? [[x y]]
            ((some-fn neg? #(> % (dec maze-size))) x y))]
    (->>
      (neighbors location)
      (remove outside-bounds?)
      (remove visited)
      (set))))

(defn- blocked-by-wall? [current-location walls neighbor]
  (walls #{current-location neighbor}))

(defn- reachable-neighbors [location visited walls]
  (let [within-maze-and-unvisited (unvisited-neighbors location visited)]
    (set
      (remove (partial blocked-by-wall? location walls)
              within-maze-and-unvisited))))

(defn- random-visitable-neighbor [location visited]
  (rand-nth (seq (unvisited-neighbors location visited))))

(defn- walls [grid doors]
  (difference grid doors))

(defn- all-locations []
  (for [x (range maze-size) y (range maze-size)] [x y]))

(defn- all-walls [location]
  (map (partial conj #{} location) (unvisited-neighbors location #{})))

(defn- fully-walled-grid []
  (reduce into #{} (map all-walls (all-locations))))

(defn generate-maze [{:keys [path visited doors size next-location-fn]
                      :or {next-location-fn random-visitable-neighbor}}]
  (defonce maze-size size)
  (if-let [current-location (peek path)]
    (if-let [next-location (next-location-fn current-location visited)]
      (recur {:path (conj path next-location)
              :visited (conj visited current-location)
              :doors (conj doors #{current-location next-location})
              :next-location-fn next-location-fn})
      (recur {:path (pop path)
              :visited (conj visited current-location)
              :doors doors
              :next-location-fn next-location-fn}))
    (walls (fully-walled-grid) doors)))

(defn solve-maze [{:keys [path visited walls size update-channel]
                   :or {update-channel nil}}]
  (let [current-location (peek path)]
    (when update-channel
      (go (>! update-channel {:walls walls :path path :visited visited})))
    (if (= current-location [(dec maze-size) (dec maze-size)])
      (do
        (when update-channel
          (go (>! update-channel :solved)))
        path)
      (if-let [next-location (rand-nth
                               (seq
                                 (reachable-neighbors current-location visited walls)))]
        (solve-maze {:path (conj path next-location)
                     :visited (conj visited current-location)
                     :walls walls
                     :update-channel update-channel})
        (solve-maze {:path (pop path)
                     :visited (conj visited current-location)
                     :walls walls
                     :update-channel update-channel})))))
