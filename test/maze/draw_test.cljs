(ns maze.draw-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require cemerick.cljs.test
            [maze.draw :as draw]))

(deftest line-test
  (testing "returns vertical line for vertical wall"
    (is (= {:x1 2 :y1 1 :x2 2 :y2 2} (draw/line (seq #{[1 1] [2 1]})))))
  (testing "returns horizontal line for a horizontal wall"
    (is (= {:x1 2 :y1 1 :x2 3 :y2 1} (draw/line (seq #{[2 0] [2 1]}))))))

