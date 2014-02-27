(ns maze.core-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require cemerick.cljs.test
            [clojure.set :refer [union]]
            [maze.core :as core]))

(deftest test-all-walls-on-permimeter
  (testing "returns all outer walls for the specified maze size"
    (is (= #{#{[0 0] [-1 0]}  #{[1 1] [2  1]}
             #{[0 0] [0 -1]}  #{[1 1] [1  2]}
             #{[1 0] [1 -1]}  #{[0 1] [0  2]}
             #{[1 0] [2  0]}  #{[0 1] [-1 1]}}
           (core/all-walls-on-perimeter 2)))))

(deftest test-neighbors
  (testing "returns all neighbors for a location"
    (is (= #{[2 1] [3 2] [2 3] [1 2]}
           (core/neighbors [2 2])))
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/neighbors [0 0])))))

(deftest test-unvisited-neighbors
  (testing "returns all neighbors when nothing has been visited"
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/unvisited-neighbors [0 0] {}))))
  (testing "returns all unvisited neighbors when neighbors have been visited"
    (is (= #{[0 -1] [-1 0]}
           (core/unvisited-neighbors [0 0] {:visited #{[0 1] [1 0]}})))))

(defn dumb-next-location [location {:keys [visited size]}]
  (cond
    (= [0 0] location) (if (visited [1 0]) nil [1 0])
    (= [1 0] location) (if (visited [1 1]) nil [1 1])
    (= [1 1] location) (if (visited [0 1]) nil [0 1])
    (= [0 1] location) nil))

(deftest test-solved?
  (testing "returns true if location is in bottom-right corner"
    (is (core/solved? [1 1] {:size 2})))
  (testing "returns false if location is not bottom-right corner"
    (not (core/solved? [0 1] {:size 2}))))

(deftest test-all-walls
  (testing "returns all walls for specified maze size"
    (is (= (union
             (core/all-walls-on-perimeter 2)
             #{#{[0 0] [0 1]} #{[0 0] [1 0]} #{[1 0] [1 1]} #{[1 1] [0 1]}})
           (core/all-walls 2)))))

(deftest test-all-walls-without-doors
  (testing "returns all the walls when there are no doors"
    (is (= (core/all-walls 2)
           (core/all-walls-without-doors {:size 2}))))
  (testing "returns all walls for the maze with doors removed"
    (is (= (union (core/all-walls-on-perimeter 2) #{#{[0 0] [0 1]}})
           (core/all-walls-without-doors {:size 2
                                          :doors #{#{[0 0] [1 0]}
                                                   #{[1 0] [1 1]}
                                                   #{[1 1] [0 1]}}})))))

(deftest test-reachable-neighbors
  (testing "returns all unvisited neighbors when there are no walls"
    (is (= (core/unvisited-neighbors [0 0] {:visited #{[1 0]}})
           (core/reachable-neighbors [0 0] {:visited #{[1 0]}}))))
  (testing "returns all unvisted neighbors that are not blocked by a wall"
    (is (= #{[-1 0] [0 -1] [1 0]}
           (core/reachable-neighbors [0 0] {:walls #{#{[0 0] [0 1]}}})))
    (is (= #{}
           (core/reachable-neighbors [0 0] {:walls #{#{[0 0] [0  1]}
                                                     #{[0 0] [1  0]}
                                                     #{[0 0] [-1 0]}
                                                     #{[0 0] [0 -1]}}})))))

(deftest test-generate-maze
  (testing "contains the correct set of walls"
    (is (= (union (core/all-walls-on-perimeter 2) #{#{[0 0] [0 1]}})
           (:walls
             (core/generate-maze {:size 2
                                  :next-location-fn dumb-next-location}))))))

(deftest test-solve-maze
  (testing "it finds a path from top-left to bottom-right"
    (is (= [[0 0] [1 0] [1 1]]
           (:path
             (core/solve-maze
               {:walls (union (core/all-walls-on-perimeter 2) #{#{[0 0] [0 1]}})
                :size 2}))))))
