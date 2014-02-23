(ns maze.draw
  (:require [maze.core :as core]))

(def maze-size-in-cells 50)
(def cell-size-in-pixels 10)
(def maze-size-in-pixels (* maze-size-in-cells cell-size-in-pixels))
(def wall-width-in-pixels 2)

(defn- make-context []
  "Creates the native js context object"
  (let [canvas (.getElementById js/document "maze-canvas")
        context (.getContext canvas "2d")]
    (set! (.-width canvas) maze-size-in-pixels)
    (set! (.-height canvas) maze-size-in-pixels)
    (set! (.-fillStyle context) "rgb(0, 0, 0)")
    context))

(defn line [[[x1 y1] [x2 y2]]]
  (let [start-point-x (max x1 x2)
        start-point-y (max y1 y2)
        [end-point-x end-point-y] (if (= x1 x2)
                                    [(inc start-point-x) start-point-y]
                                    [start-point-x (inc start-point-y)])]
    {:x1 start-point-x :y1 start-point-y :x2 end-point-x :y2 end-point-y}))

(defn draw-line [{:keys [x1 y1 x2 y2]} context]
  (doto context
    (.beginPath)
    (.moveTo (* x1 cell-size-in-pixels) (* y1 cell-size-in-pixels))
    (.lineTo (* x2 cell-size-in-pixels) (* y2 cell-size-in-pixels))
    (.stroke)))

(defn start []
  (let [walls (core/generate-maze {:visited #{}
                                   :path [[0 0]]
                                   :doors #{}
                                   :size maze-size-in-cells})
        lines (map line (map seq walls))
        context (make-context)]
    (doseq [line lines]
      (draw-line line context))))
