(ns erv.cps.core-test
  (:require
   [erv.cps.core :as cps :refer [->cps
                                 maps->data
                                 filter-scale
                                 set->maps
                                 bound-ratio
                                 find-subcps]]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest testing is]]
   [clojure.set :as set]))

(->cps 1 #{1 2 3})
(->cps 4 [1 2 3])
(->cps 3 [1 2 3 5])
(deftest ->cps-test
  (testing "Creates all possible combinations of the given generators"
    (is (= #{#{1 3} #{1 2} #{3 2}}
           (->cps 2 [1 2 3])))
    (is (= #{#{1 2 3}}
           (->cps 3 [1 2 3])))
    (is (= #{#{3 2 5} #{1 3 5} #{1 2 5} #{1 3 2}}
           (->cps 3 [1 2 3 5]))))
  (testing "Can take a set as an input"
    (is (= #{#{1} #{2} #{3}}
           (->cps 1 #{1 2 3})))
    (is (= #{#{1 3} #{1 2} #{3 2}}
           (->cps 2 #{1 2 3}))))
  (testing "If `size` is 0, it returns #{#{}}"
    (is (= #{#{}}
           (->cps 0 #{1 2 3}))))
  (testing "If `size` is > than `(count generators)`, it returns `#{#{}}` "
    (is (= #{#{}}
           (->cps 4 #{1 2 3})))))

(deftest filter-scale-test
  (let [hex (->> [1 3 5 7] (->cps 2) set->maps (bound-ratio 2)
                 (maps->data :bounded-ratio) :scale) ]
    (is (= #{#{7 1} #{1 5} #{1 3}}
           (->> (filter-scale hex #{1}) (map :set) set)))

    (is (= #{#{7 1} #{1 5} #{1 3} #{3 5} #{3 7}}
           (->> (filter-scale hex #{1 3}) (map :set) set)))))

(deftest find-subcps-test
  (testing "It returns a `::sub-cps-set` (set of CPSs), therefore guaranteeing uniqueness"
    (is (s/valid? ::cps/sub-cps-set (find-subcps [1 2 3 4 5 6] 3 2 4)))
    (is (s/valid? ::cps/sub-cps-set (find-subcps [1 2 3 4 5 6 7 8] 3 2 4))))

  (testing "There are 30 hexanies in an eikosany"
    (is (= 30 (count (find-subcps [1 2 3 4 5 6] 3 2 4)))))

  (testing "There are 30 tetrads (major and minor) in an eikosay"
    (is (= 30 (count (set/union (find-subcps [1 2 3 4 5 6] 3 1 4)
                                (find-subcps [1 2 3 4 5 6] 3 3 4)))))))
