(ns clj-billiards.vector-test
  (:require [clojure.test :refer :all])
  (:require [clj-billiards.vector :refer :all])
  (:require [clj-billiards.test-helpers :refer [rel= v-rel=]]))

(deftest ReportingLearningTest
  (testing "Rel="
    (is (rel= (dot {:x 0.333 :y 3.5} {:x 4.5 :y 2.333})
              9.664
              0.0001))))
(deftest ReportingLearningOtherTest
  (testing "V Rel="
    (is (v-rel= (subtract {:x 0.333 :y 3.5} {:x 4.5 :y 2.333})
                {:x -4.167 :y 1.166}
                0.001))))

(deftest VectorOperationsTest
  (testing "Dot product"
    (is (= (dot {:x 2 :y 3} {:x 4 :y 5}) 23))
    (is (= (dot {:x 0 :y 3} {:x 4 :y 5}) 15))
    (is (= (dot {:x 0 :y 3} {:x 4 :y 0}) 0))
    (is (rel= (dot {:x 0.333 :y 3.5} {:x 4.5 :y 2.333}) 9.664 0.0001))
    )
  (testing "Subtraction"
    (is (= (subtract {:x 2 :y 3} {:x 4 :y 5}) {:x -2 :y -2}))
    (is (= (subtract {:x 0 :y 3} {:x 4 :y 5}) {:x -4 :y -2}))
    (is (= (subtract {:x 0 :y 3} {:x 4 :y 0}) {:x -4 :y 3}))
    (is (v-rel= (subtract {:x 0.333 :y 3.5} {:x 4.5 :y 2.333}) {:x -4.1667 :y 1.1667} 0.001))
    )
  (testing "Scale"
    (is (= (scale 2 {:x 2 :y 3}) {:x 4 :y 6}))
    (is (= (scale -3 {:x 0 :y 3}) {:x 0 :y -9}))
    (is (v-rel= (scale 3 {:x 3.333333 :y 3.333333}) {:x 10 :y 10} 0.001))
    (is (v-rel= (scale 5 {:x 0.333 :y 3.5}) {:x 1.665 :y 17.5} 0.001))
    )
  (testing "Magnitude"
    (is (= (magnitude {:x 3 :y 4}) 5.0))
    (is (= (magnitude {:x 0 :y 3}) 3.0))
    (is (rel= (magnitude {:x 4.5 :y 2.333}) 5.068 0.001))))

(deftest CollisionsTest
  (testing "Collision"                                      ; Note: Something is going wrong here
    ; Direct collision into stationary ball
    (is (v-rel= (collision-v1 {:x 10 :y 0} {:x 0 :y 0} {:x 0 :y 0} {:x 7.25 :y 0.0}) {:x 0 :y 0} 0.001))
    (is (v-rel= (collision-v2 {:x 10 :y 0} {:x 0 :y 0} {:x 0 :y 0} {:x 7.25 :y 0.0}) {:x 10 :y 0} 0.001))
    ; Direct collision into slightly askew stationary ball
    (is (v-rel= (collision-v1 {:x 10 :y 0} {:x 0 :y 0} {:x 0 :y 0} {:x 7.25 :y 2.0}) {:x 0.707 :y -2.563} 0.001))
    (is (v-rel= (collision-v2 {:x 10 :y 0} {:x 0 :y 0} {:x 0 :y 0} {:x 7.25 :y 2.0}) {:x 9.293 :y 2.563} 0.001))
    ; Angled collision of 2 moving balls
    (is (v-rel= (collision-v1 {:x 10 :y 0} {:x 5 :y -5} {:x 0 :y 0} {:x 7.23 :y 7.23}) {:x 5 :y -5} 0.001))
    (is (v-rel= (collision-v2 {:x 10 :y 0} {:x 5 :y -5} {:x 0 :y 0} {:x 7.23 :y 7.23}) {:x 10 :y 0} 0.001)))
  )
