(ns maze.top-level
  (:require [maze.core :as core]
            [maze.draw :as draw]))

(def maze
  (core/generate-maze {:visited #{}
                       :path [[0 0]]
                       :doors #{}
                       :size draw/maze-size}))

(defn start []
  (core/solve-maze {:path [[0 0]]
                    :visited #{}
                    :walls maze
                    :size draw/maze-size
                    :update-fn (partial draw/update-canvas (draw/make-context))}))
