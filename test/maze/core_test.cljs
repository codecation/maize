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
           (core/outer-walls 2)))
    (is (= #{#{[0 0] [0 -1]} #{[1 0] [1 -1]} #{[2 0] [2 -1]}
             #{[0 0] [-1 0]}                 #{[2 0] [3  0]}
             #{[0 1] [-1 1]}                 #{[2 1] [3  1]}
             #{[0 2] [-1 2]}                 #{[2 2] [3  2]}
             #{[0 2] [0  3]} #{[1 2] [1  3]} #{[2 2] [2  3]}}
           (core/outer-walls 3)))))

(deftest test-all-locations-visited?
  (testing "returns true when the path is empty"
    (is (core/all-locations-visited? {:path []})))
  (testing "returns false when the path is not empty"
    (is (not (core/all-locations-visited? {:path [[0 0]]})))))

(deftest test-neighbors
  (testing "returns all neighbors for a location"
    (is (= #{[2 1] [3 2] [2 3] [1 2]}
           (core/neighbors [2 2])))
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/neighbors [0 0])))))

(deftest test-unvisited-neighbors
  (testing "returns all neighbors when nothing has been visited"
    (is (= #{[0 -1] [1 0] [0 1] [-1 0]}
           (core/unvisited-neighbors {:location [0 0] :visited #{}}))))
  (testing "returns all unvisited neighbors when neighbors have been visited"
    (is (= #{[0 -1] [-1 0]}
           (core/unvisited-neighbors {:location [0 0] :visited #{[0 1] [1 0]}})))))

(defn dumb-next-location [{:keys [location visited size]}]
  (cond
    (= [0 0] location) (if (visited [1 0]) nil [1 0])
    (= [1 0] location) (if (visited [1 1]) nil [1 1])
    (= [1 1] location) (if (visited [0 1]) nil [0 1])
    (= [0 1] location) nil))

(deftest test-solved?
  (testing "returns true if location is in bottom-right corner"
    (is (core/solved?  {:location [1 1] :size 2})))
  (testing "returns false if location is not bottom-right corner"
    (not (core/solved? {:location [0 1] :size 2}))))

(deftest all-walls
  (testing "returns all walls when there are no doors"
    (is (= (union
             (core/outer-walls 2)
             #{#{[0 0] [1 0]} #{[0 0] [0 1]} #{[1 0] [1 1]} #{[1 1] [0 1]}})
           (core/all-walls {:walls (core/outer-walls 2)
                            :doors #{}}))))
  (testing "returns all walls with doors removed"
    (is (= (union (core/outer-walls 2) #{#{[0 0] [0 1]}})
           (core/all-walls {:walls (core/outer-walls 2)
                            :doors #{#{[0 0] [1 0]}
                                     #{[1 0] [1 1]}
                                     #{[1 1] [0 1]}}})))))

(deftest test-reachable-neighbors
  (testing "returns all unvisited neighbors when there are no walls"
    (is (= (core/unvisited-neighbors {:location [0 0]
                                      :visited #{[1 0]}})
           (core/reachable-neighbors {:location [0 0]
                                      :visited #{[1 0]}
                                      :walls #{}}))))
  (testing "returns all unvisted neighbors that are not blocked by a wall"
    (is (= #{[-1 0] [0 -1] [1 0]}
           (core/reachable-neighbors {:location [0 0]
                                      :visited #{}
                                      :walls #{#{[0 0] [0 1]}}})))
    (is (= #{}
           (core/reachable-neighbors {:location [0 0]
                                      :visited #{}
                                      :walls #{#{[0 0] [0  1]}
                                               #{[0 0] [1  0]}
                                               #{[0 0] [-1 0]}
                                               #{[0 0] [0 -1]}}})))))

(deftest test-generate-maze
  (testing "contains the correct set of walls"
    (is (= (union (core/outer-walls 2) #{#{[0 0] [0 1]}})
           (:walls
             (core/generate-maze {:size 2
                                  :next-location-fn dumb-next-location}))))))

(deftest test-solve-maze
  (testing "it finds a path from top-left to bottom-right"
    (let [maze (core/generate-maze {:size 2
                                    :next-location-fn dumb-next-location})]
      (is (= [[0 0] [1 0] [1 1]]
             (:path (core/solve-maze {:walls (:walls maze)
                                      :size (:size maze)})))))))
