(ns maze.core-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require cemerick.cljs.test
            [maze.core :as core]))

(deftest test-neighbors
  (testing "returns all neighbors for a location"
    (is (= #{[2 1] [3 2] [2 3] [1 2]}
           (core/neighbors [2 2])))
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/neighbors [0 0])))))

(deftest test-unvisited-neighbors
  (testing "returns a set of neighbors within bounds of maze"
    (is (= #{[1 0] [0 1]}
           (core/unvisited-neighbors [0 0] #{} 5)))
    (is (= #{[4 3] [3 4]}
           (core/unvisited-neighbors [4 4] #{} 5))))
  (testing "returns all unvisited neighbors"
    (is (= #{[2 3] [1 2]}
           (core/unvisited-neighbors [2 2] #{[2 1] [3 2]} 5)))))

(defn dumb-next-location [location visited size]
  (cond
    (= [0 0] location) (if (visited [1 0]) nil [1 0])
    (= [1 0] location) (if (visited [1 1]) nil [1 1])
    (= [1 1] location) (if (visited [0 1]) nil [0 1])
    (= [0 1] location) nil))

(deftest test-generate-maze
  (testing "returns a set of walls"
    (is (= #{#{[0 0] [0 1]}}
           (core/generate-maze {:visited #{}
                                :path [[0 0]]
                                :doors #{}
                                :size 2
                                :next-location-fn dumb-next-location })))))

(deftest test-solved-location?
  (testing "returns true if location is in bottom-right corner"
    (is (core/solved-location? [1 1] 2)))
  (testing "returns false if location is not bottom-right corner"
    (not (core/solved-location? [0 1] 2))))

(deftest test-fully-walled-grid
  (testing "returns all walls for specified grid size"
    (is (= #{#{[0 0] [0 1]} #{[0 0] [1 0]} #{[1 0] [1 1]} #{[1 1] [0 1]}}
           (core/fully-walled-grid 2)))))

(deftest test-walls-without-doors
  (testing "returns all the walls when there are no doors"
    (is (= #{#{[0 0] [1 0]}}
           (core/walls-without-doors #{#{[0 0] [1 0]}} #{}))))
  (testing "returns the walls with doors removed"
    (is (= #{#{[0 0] [1 0]}}
           (core/walls-without-doors #{#{[2 2] [2 3]} #{[0 0] [1 0]}}
                       #{#{[2 2] [2 3]}})))))

(deftest test-reachable-neighbors
  (testing "returns the set of neighbors that are within the maze, unvisited
           and not blocked by walls"
    (is (= #{[1 0]} (core/reachable-neighbors [0 0] #{} #{#{[0 0] [0 1]}} 2)))
    (is (= #{} (core/reachable-neighbors [0 0] #{} #{#{[0 0] [0 1]} #{[0 0] [1 0]}} 2)))))

(deftest test-solve-maze
  (testing "it finds a path from top-left to bottom-right"
    (is (= [[0 0] [1 0] [1 1]]
           (core/solve-maze {:visited #{}
                             :path [[0 0]]
                             :walls #{#{[0 0] [0 1]}}
                             :size 2})))))
