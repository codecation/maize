(ns maze.top-level
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [maze.core :as core]
            [maze.draw :as draw]
            [cljs.core.async :refer [chan timeout]]))

(def delay-between-iterations 10)

(defn new-maze []
  (core/generate-maze {:size draw/maze-size}))

(def context
  (delay
    (draw/make-context)))

(defn ^:export start []
  (let [update-channel (chan)]
    (go
      (while true
        (let [update-contents (<! update-channel)
              finished? #(= update-contents :finished)]
          (<! (timeout delay-between-iterations))
          (if (finished?)
            (start)
            (draw/update-canvas @context update-contents)))))
    (draw/clear-canvas @context)
    (core/solve-maze {:walls (:walls (new-maze))
                      :size draw/maze-size
                      :update-channel update-channel})))
