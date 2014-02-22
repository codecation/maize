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
