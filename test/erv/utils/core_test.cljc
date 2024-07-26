(ns erv.utils.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.core :refer [pattern->degrees pick-degrees pick-pattern]]))

(deftest pattern->indexes-test
  (is (= [0 2 4 5 7 9 11]
         (pattern->degrees [2 2 1 2 2 2 1]))))

(deftest pick-pattern-test
  (is (= [0 2 4 5 7 9 11]
         (pick-pattern [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14]
                       [2 2 1 2 2 2 1])))
  (is (= [0 2 4 5 7]
         (pick-pattern [0 1 2 3 4 5 6 7 8]
                       [2 2 1 2 2 2 1]))))

(deftest pick-degrees-test
  (is (= [0 1 2 3 4 0 1 2 3 4]
         (pick-degrees
          (range 5)
          (range 10)))))
