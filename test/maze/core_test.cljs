(ns maze.core-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require cemerick.cljs.test
            [clojure.set :refer [union]]
            [maze.core :as core]))

(deftest test-outer-walls
  (testing "returns all outer walls for the specified maze size"
    (is (= #{#{[0 0] [0 -1]} #{[1 0] [1 -1]}
             #{[0 0] [-1 0]} #{[1 0] [2  0]}
             #{[0 1] [0  2]} #{[1 1] [2  1]}
             #{[0 1] [-1 1]} #{[1 1] [1  2]}}
           (core/outer-walls {:size 2})))
    (is (= #{#{[0 0] [0 -1]} #{[1 0] [1 -1]} #{[2 0] [2 -1]}
             #{[0 0] [-1 0]}                 #{[2 0] [3  0]}
             #{[0 1] [-1 1]}                 #{[2 1] [3  1]}
             #{[0 2] [-1 2]}                 #{[2 2] [3  2]}
             #{[0 2] [0  3]} #{[1 2] [1  3]} #{[2 2] [2  3]}}
           (core/outer-walls {:size 3})))))

(deftest test-all-locations-visited?
  (testing "returns true when all locations have been visited"
    (is (core/all-locations-visited? {:visited #{[0 1]}
                                      :size 1})))
  (testing "returns false when all locations have not been visited"
    (is (not (core/all-locations-visited? {:visited #{}
                                           :size 1})))))

(deftest test-neighbors
  (testing "returns all neighbors for a location"
    (is (= #{[2 1] [3 2] [2 3] [1 2]}
           (core/neighbors [2 2])))
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/neighbors [0 0])))))

(deftest test-unvisited-neighbors
  (testing "returns all neighbors when nothing has been visited"
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/unvisited-neighbors {:current-path [[0 0]]
                                      :visited #{}}))))
  (testing "returns all unvisited neighbors when neighbors have been visited"
    (is (= #{[0 -1] [-1 0]}
           (core/unvisited-neighbors {:current-path [[0 0]]
                                      :visited #{[0 1] [1 0]}})))))

(deftest test-solved?
  (testing "returns true if location is bottom-right corner"
    (is (core/solved?  {:current-path [[1 1]]
                        :size 2})))
  (testing "returns false if location is not bottom-right corner"
    (not (core/solved? {:current-path [[0 1]]
                        :size 2}))))

(deftest test-add-inner-walls
  (testing "returns all walls when there are no doors"
    (is (= (union
             (core/outer-walls {:size 2})
             #{#{[0 0] [1 0]} #{[0 0] [0 1]} #{[1 0] [1 1]} #{[1 1] [0 1]}})
           (core/add-inner-walls {:size 2
                                  :walls (core/outer-walls {:size 2})
                                  :doors #{}}))))
  (testing "returns all walls with doors removed"
    (is (= (union (core/outer-walls {:size 2}) #{#{[0 0] [0 1]}})
           (core/add-inner-walls {:size 2
                                  :walls (core/outer-walls {:size 2})
                                  :doors #{#{[0 0] [1 0]}
                                           #{[1 0] [1 1]}
                                           #{[1 1] [0 1]}}})))))

(deftest test-reachable-neighbors
  (testing "returns all unvisited neighbors when there are no walls"
    (is (= (core/unvisited-neighbors {:current-path [[0 0]]
                                      :visited #{[1 0]}})
           (core/reachable-neighbors {:current-path [[0 0]]
                                      :visited #{[1 0]}
                                      :walls #{}}))))
  (testing "returns all unvisted neighbors that are not blocked by a wall"
    (is (= #{[-1 0] [0 -1] [1 0]}
           (core/reachable-neighbors {:current-path [[0 0]]
                                      :visited #{}
                                      :walls #{#{[0 0] [0 1]}}})))
    (is (= #{}
           (core/reachable-neighbors {:current-path [[0 0]]
                                      :visited #{}
                                      :walls #{#{[0 0] [0  1]}
                                               #{[0 0] [1  0]}
                                               #{[0 0] [-1 0]}
                                               #{[0 0] [0 -1]}}})))))

(deftest test-depth-first
  (testing "returns paths as a vector (stack) in the correct order"
    (is (= [[[0 0]] [[0 1]]]
           (core/depth-first [[[0 0]] [[0 1]]])))
    (is (= [[[0 0]] [[0 1]]]
           (core/depth-first '([[0 0]] [[0 1]]))))))

(deftest test-breadth-first
  (testing "returns paths as a list (queue) in the correct order"
    (is (= '([[0 1]] [[0 0]])
           (core/breadth-first '([[0 0]] [[0 1]]))))
    (is (= '([[0 1]] [[0 0]])
           (core/breadth-first [[[0 0]] [[0 1]]])))))

(deftest test-possible-paths
  (let [outer-walls (core/outer-walls {:size 2})]
    (testing "returns all reachable paths from current path"
      (is (= [[[0 0] [0 1]] [[0 0] [1 0]]]
             (core/possible-paths {:current-path [[0 0]]
                                   :visited #{}
                                   :walls outer-walls}))))
    (testing "does not include paths to visited locations"
      (is (= [[[0 0] [1 0]]]
             (core/possible-paths {:current-path [[0 0]]
                                   :visited #{[0 1]}
                                   :walls outer-walls}))))
    (testing "does not include paths through walls"
      (is (= [[[0 0] [0 1]]]
             (core/possible-paths {:current-path [[0 0]]
                                   :visited #{}
                                   :walls (union outer-walls
                                                 #{#{[0 0] [1 0]}})}))))))

(defn deterministic-possible-paths [{:keys [current-path visited]}]
  (let [current-location (peek current-path)]
    (cond
      (= [0 0] current-location) (if (visited [1 0]) [] [(conj current-path [1 0])])
      (= [1 0] current-location) (if (visited [1 1]) [] [(conj current-path [1 1])])
      (= [1 1] current-location) (if (visited [0 1]) [] [(conj current-path [0 1])])
      (= [0 1] current-location) [])))

(deftest test-generate-maze
  (testing "contains the correct set of walls"
    (is (= (union (core/outer-walls {:size 2}) #{#{[0 0] [0 1]}})
           (:walls
             (core/generate-maze {:size 2
                                  :possible-paths-fn deterministic-possible-paths}))))))

(deftest test-solve-maze
  (testing "it finds a path from top-left to bottom-right"
    (let [maze (core/generate-maze {:size 2
                                    :possible-paths-fn deterministic-possible-paths})]
      (is (= [[0 0] [1 0] [1 1]]
             (:current-path (core/solve-maze {:walls (:walls maze)
                                              :size (:size maze)})))))))
