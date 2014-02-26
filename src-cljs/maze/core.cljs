(ns maze.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set :refer [difference]]))

(declare maze-size)

(defn- neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
      [(+ x dx) (+ y dy)])))

(defn- unvisited-neighbors [location {:keys [visited]}]
  (letfn [(outside-bounds? [[x y]]
            ((some-fn neg? #(> % (dec maze-size))) x y))]
    (->>
      (neighbors location)
      (remove outside-bounds?)
      (remove visited)
      (set))))

(defn- blocked-by-wall? [current-location walls neighbor]
  (walls #{current-location neighbor}))

(defn- reachable-neighbors [location {:keys [visited walls]
                                      :or {walls {}}}]
  (let [within-maze-and-unvisited (unvisited-neighbors location {:visited visited})]
    (set
      (remove (partial blocked-by-wall? location walls)
              within-maze-and-unvisited))))

(defn- all-walls []
  (let [all-walls-for-location (fn [location] (map (partial conj #{} location)
                                                   (unvisited-neighbors location {:visited #{}})))
        all-locations (fn [] (for [x (range maze-size) y (range maze-size)] [x y]))]
    (reduce into #{} (map all-walls-for-location (all-locations)))))

(defn- all-walls-without-doors [doors]
  (difference (all-walls) doors))

(defn- all-locations-visited? [location {:keys [visited]}]
  (= (count visited) (* maze-size maze-size)))

(defn- solved? [location]
  (= location [(dec maze-size) (dec maze-size)]))

(defn- random-reachable-neighbor [location {:keys [visited walls size]
                                            :or {walls {}}}]
  (rand-nth (seq (reachable-neighbors location {:visited visited
                                                :walls walls
                                                :size size}))))

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
          (merge maze {:walls (all-walls-without-doors doors)}))
        (let [maze (merge maze {:visited (conj visited current-location)})]
          (if-let [next-location (next-location-fn current-location maze)]
            (search-maze (merge maze {:path (conj path next-location)
                                      :doors (conj doors #{current-location next-location})}))
            (search-maze (merge maze {:path (pop path)}))))))))

(defn generate-maze [maze]
  (defonce maze-size (:size maze))
  (search-maze (merge maze {:finished-fn all-locations-visited?})))

(defn solve-maze [maze]
  (search-maze (merge maze {:finished-fn solved?})))
