(ns maze.draw)

(def maze-size 20)
(def cell-size-in-pixels 25)
(def maze-size-in-pixels (* maze-size cell-size-in-pixels))
(def wall-width-in-pixels 2)

(defn- make-context []
  "Creates the native js context object"
  (let [canvas (.getElementById js/document "maze-canvas")
        context (.getContext canvas "2d")]
    (set! (.-width canvas) maze-size-in-pixels)
    (set! (.-height canvas) maze-size-in-pixels)
    (set! (.-fillStyle context) "rgb(0, 0, 0)")
    (set! (.-lineWidth context) wall-width-in-pixels)
    context))

(defn line [[[x1 y1] [x2 y2]]]
  (let [start-point-x (max x1 x2)
        start-point-y (max y1 y2)
        cells-horizontally-adjacent? (= x1 x2)
        [end-point-x end-point-y] (if cells-horizontally-adjacent?
                                    [(inc start-point-x) start-point-y]
                                    [start-point-x (inc start-point-y)])]
    {:x1 start-point-x :y1 start-point-y :x2 end-point-x :y2 end-point-y}))

(defn draw-line [{:keys [x1 y1 x2 y2]} context]
  (doto context
    (.beginPath)
    (.moveTo (* x1 cell-size-in-pixels) (* y1 cell-size-in-pixels))
    (.lineTo (* x2 cell-size-in-pixels) (* y2 cell-size-in-pixels))
    (.stroke)))

(defn set-draw-color [color context]
  (set! (.-fillStyle context) color))

(defn draw-walls [walls context]
  (let [lines (map line (map seq walls))]
    (doseq [line lines]
      (draw-line line context))))

(defn fill-location [[x y] context]
  (.fillRect context
             (* x cell-size-in-pixels)
             (* y cell-size-in-pixels)
             cell-size-in-pixels
             cell-size-in-pixels))

(defn draw-locations [locations context]
  (doseq [location locations]
    (fill-location location context)))

(defn update-canvas [context {:keys [current-path visited walls]}]
  (set-draw-color "rgb(255, 180, 180)" context)
  (draw-locations visited context)

  (set-draw-color "rgb(180, 255, 180)" context)
  (draw-locations current-path context)

  (set-draw-color "rgb(100, 255, 100)" context)
  (draw-locations [(peek current-path)] context)

  (set-draw-color "rgb(0,0,0)" context)
  (draw-walls walls context))

(defn clear-canvas [context]
  (let [canvas (.-canvas context)]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))))
