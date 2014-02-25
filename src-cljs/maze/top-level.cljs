(ns maze.top-level
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [maze.core :as core]
            [maze.draw :as draw]
            [cljs.core.async :refer [chan timeout]]))

(def delay-between-iterations 10)
(def maze-size 20)

(defn new-maze []
  (core/generate-maze {:visited #{}
                       :path [[0 0]]
                       :doors #{}
                       :size maze-size}))

(def context
  (delay
    (draw/make-context)))

(defn ^:export start []
  (let [update-channel (chan)]
    (go
      (while true
        (let [update-contents (<! update-channel)
              solved? #(= update-contents :solved)]
          (<! (timeout delay-between-iterations))
          (if (solved?)
            (start)
            (draw/update-canvas @context update-contents)))))
    (draw/clear-canvas @context)
    (core/solve-maze {:path [[0 0]]
                      :visited #{}
                      :walls (new-maze)
                      :size maze-size
                      :update-channel update-channel})))
