(ns maze.core
  (:require [maze.draw :as draw]
            clojure.set))

(defn neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn unvisited-neighbors [location visited size]
  (letfn [(outside-bounds? [[x y]]
            ((some-fn neg? #(> % (dec size))) x y))]
    (->>
      (neighbors location)
      (remove outside-bounds?)
      (remove visited)
      (set))))

(defn- blocked-by-wall? [current-location walls neighbor]
  (walls #{current-location neighbor}))

(defn reachable-neighbors [location visited walls size]
  (let [within-maze-and-unvisited (unvisited-neighbors location visited size)]
    (set
      (remove (partial blocked-by-wall? location walls) 
              within-maze-and-unvisited))))

(defn- random-visitable-neighbor [location visited size]
  (rand-nth (seq (unvisited-neighbors location visited size))))

(defn- walls [grid doors]
  (clojure.set/difference grid doors))

(defn- all-locations [size]
  (for [x (range size) y (range size)] [x y]))

(defn- all-walls [size location]
  (map (partial conj #{} location) (unvisited-neighbors location #{} size)))

(defn- fully-walled-grid [size]
  (reduce into #{} (map (partial all-walls size) (all-locations size))))

(defn generate-maze [{:keys [path visited doors size next-location-fn]
                      :or {next-location-fn random-visitable-neighbor}}]
  (if-let [current-location (peek path)]
    (if-let [next-location (next-location-fn current-location visited size)]
      (recur {:path (conj path next-location)
              :visited (conj visited current-location)
              :doors (conj doors #{current-location next-location})
              :size size
              :next-location-fn next-location-fn})
      (recur {:path (pop path)
              :visited (conj visited current-location)
              :doors doors
              :size size
              :next-location-fn next-location-fn}))
    (walls (fully-walled-grid size) doors)))

(defn solve-maze [{:keys [path visited walls size update-fn]
                   :or {update-fn #()}}]
  (let [current-location (peek path)]
    (update-fn {:walls walls :path path :visited visited})
    (if (= current-location [(dec size) (dec size)])
      path
      (if-let [next-location (rand-nth
                               (seq
                                 (reachable-neighbors current-location visited walls size)))]
        (recur {:path (conj path next-location)
                :visited (conj visited current-location)
                :walls walls
                :size size
                :update-fn update-fn})
        (recur {:path (pop path)
                :visited (conj visited current-location)
                :walls walls
                :size size
                :update-fn update-fn})))))
