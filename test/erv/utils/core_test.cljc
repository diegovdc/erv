(ns erv.utils.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.core :refer [make-map-by-key pattern->degrees pick-degrees
                           pick-pattern]]))

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

(deftest make-map-by-key-test
  (is (= {1 {:id 1}, 2 {:id 2}}
         (make-map-by-key :id [{:id 1} {:id 2}]))))
