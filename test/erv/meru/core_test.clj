(ns erv.meru.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.meru.core :refer [recurrent-series]]))

(deftest recurrent-series-test
  (let [meta-slendro-series
        {:convergence 1.324717957244746,
         :converges-at 84,
         :series [1 1 1 2 2 3 4 5 7 9 12 16 21 28 37 49 65 86 114 151 200 265 351 465 616 816 1081 1432 1897 2513 3329 4410 5842 7739 10252 13581 17991 23833 31572 41824 55405 73396 97229 128801 170625 226030 299426 396655 525456 696081 922111 1221537 1618192 2143648 2839729 3761840 4983377 6601569 8745217 11584946 15346786 20330163 26931732 35676949 47261895 62608681 82938844 109870576 145547525 192809420 255418101 338356945 448227521 593775046 786584466 1042002567 1380359512 1828587033 2422362079 3208946545 4250949112 5631308624 7459895657 9882257736 13091204281 17342153393 22973462017 30433357674 40315615410 53406819691]}]
    (testing "Can use a `:seed`  and a `:formula`"
      (is (= meta-slendro-series
             (recurrent-series {:seed [1 1 1]
                                :formula :meta-slendro}))))
    (testing "Can use a `:seed`  and a custom configuration of indexes and an operation for apply the values of the indexes."
      (is (= meta-slendro-series
             (recurrent-series {:seed [1 1 1]
                                :i1 2
                                :i2 3
                                :f (fn [a b] (+ a b))}))))))
