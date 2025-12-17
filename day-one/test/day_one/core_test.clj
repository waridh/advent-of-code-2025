(ns day-one.core-test
  (:require [clojure.test :refer :all]
            [day-one.core :refer :all]))

(def given-test-input ["L68"
                       "L30"
                       "R48"
                       "L5"
                       "R60"
                       "L55"
                       "L1"
                       "L99"
                       "R14"
                       "L82"])

(deftest data-test
  (testing "instance check"
    (is (Left? (->Left 67)))
    (is (Right? (->Right 3))))
  (testing "single count zeros"
    (is (= (count-zeros-aux [7 93] (->Right 7)) [8 0]))
    (is (= (count-zeros-aux [0 7] (->Left 7)) [1 0]))
    (is (= (count-zeros-aux [7 9] (->Left 7)) [7 2]))
    (is (= (count-zeros-aux [7 9] (->Right 7)) [7 16])))
  (testing "conversion"
    (is (= (string->command "L47") (->Left 47)))
    (is (= (string->command "R88") (->Right 88))))
  (testing "end to end"
    (is (= (strings->count-zeros given-test-input)
           3))))

(deftest part-2
  (testing "test-count-clicks-aux"
    (is (= (count-clicks-aux [7 93] (->Right 10)) [8 3]))
    (is (= (count-clicks-aux [7 93] (->Right 7)) [8 0]))
    (is (= (count-clicks-aux [7 3] (->Left 10)) [8 93]))
    (is (= (count-clicks-aux [7 93] (->Left 93)) [8 0]))
    (is (= (count-clicks-aux [0 0] (->Left 100)) [1 0]))
    (is (= (count-clicks-aux [0 0] (->Left 99)) [0 1]))
    (is (= (count-clicks-aux [0 0] (->Right 100)) [1 0]))
    (is (= (count-clicks-aux [0 0] (->Left 1000)) [10 0]))))
