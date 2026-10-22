(ns erv.mos.mos-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.mos.mos :as subject]))

(deftest make-test
  (is (= [[12]
          [7 5]
          [2 5 5]
          [2 2 3 2 3]
          [2 2 2 1 2 2 1]
          [1 1 1 1 1 1 1 1 1 1 1 1]]
         (subject/make 12 7))))
