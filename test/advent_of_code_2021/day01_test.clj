(ns advent-of-code-2021.day01-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-2021.util :refer [read-numbers]]
            [advent-of-code-2021.day01 :refer [count-increasing]]))

(deftest test-day01-1
  (testing "Result 1 of day 1 is correct"
    (let [filename "day_01/input_01.txt"
          measurements (read-numbers filename)]

      (is 1655 (count-increasing measurements 1))
      (is 1683 (count-increasing measurements 3)))))