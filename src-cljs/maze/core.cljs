(ns maze.core)

;; doors - set of sets of pairs of vecs
;;  #{
;;     #{[1 2] [2 2]}
;;     #{[3 3] [3 4]}
;;   }
;;
;; visited locations - set
;;
;; start fully walled
;;
;; start in upper left
;; mark as visited
;; randomly select adjacent cell to walk to
;; add selected 

; {
;   :doors   => set of sets of locations
;   :visited => set of locations
;   :path    => stack of locations
;   :size    => size of maze
; }

; 

(defn neighbors [[x y]]
  (set
    (for [dx [-1 0 1] dy [-1 0 1]
          :when (not= (.abs js/Math dx) (.abs js/Math dy))]
    [(+ x dx) (+ y dy)])))

(defn visitable-neighbors [location visited size]
  (letfn [(outside-bounds [[x y]]
           ((some-fn neg? #(> % (dec size))) x y))]
    (->>
      (neighbors location)
      (remove outside-bounds)
      (remove visited)
      (set))))

(defn generate-maze [{:keys [path visited doors size]}]
  {:visited (range (* size size))})

; (defn start []
;   (generate-maze [[0,0]]))
