(ns maze.top-level
  (:require [maze.draw :as draw]))

(defn start []
  (let [walls (draw/actually-generate-maze)]
    (draw/draw-walls walls)))
