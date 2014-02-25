(ns maze.top-level
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [maze.core :as core]
            [maze.draw :as draw]
            [cljs.core.async :refer [chan timeout]]))

(def delay-between-iterations 1)

(def maze
  (core/generate-maze {:visited #{}
                       :path [[0 0]]
                       :doors #{}
                       :size draw/maze-size}))

(def context
  (delay
    (draw/make-context)))

(defn start []
  (let [update-channel (chan)]
    (go
      (while true
        (let [update-contents (<! update-channel)
              solved? #(= update-contents :solved)]
          (<! (timeout delay-between-iterations))
          (if (solved?)
            (start)
            (draw/update-canvas @context update-contents)))))
    (core/solve-maze {:path [[0 0]]
                      :visited #{}
                      :walls maze
                      :size draw/maze-size
                      :update-channel update-channel})))
