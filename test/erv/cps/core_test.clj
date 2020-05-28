(ns erv.cps.core-test
  (:require
   [erv.cps.core :as cps :refer [->cps
                                 maps->data
                                 filter-scale
                                 set->maps
                                 bound-ratio
                                 find-subcps
                                 get-cps-description
                                 subcps-sets->map]]
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

(deftest get-cps-description-test
  (testing "Creates a string representation of a CPS"
    (is (= "1.2.3.4" (get-cps-description (->cps 2 [1 2 3 4])))))
  (testing "Deals with fractions as generatos"
    (is (= "1/4.2.3.4" (get-cps-description (->cps 2 [1/4 2 3 4])))))
  (testing "Can represent common generators shared across a whole set (helpful for subcps sets)"
    (is (= "9-1.5.7.11" (get-cps-description
                         #{#{7 9 5} #{11 9 5} #{1 11 9}
                           #{7 11 9} #{1 9 5} #{7 1 9}})))
    (is (= "7.13-1.9.11.15" (get-cps-description
                             #{#{7 15 13 11} #{7 15 13 9} #{7 1 13 11}
                               #{7 1 15 13} #{7 1 13 9} #{7 13 11 9}})))))

(deftest subcps-sets->map-test
  (testing "Converts a set of subcps sets into a map where keys are the descriptions of the subcps sets"
    (is (= (subcps-sets->map (find-subcps [1 3 5 7] 2 1 3))
           {"5-1.3.7" #{#{7 5} #{3 5} #{1 5}},
            "3-1.5.7" #{#{3 5} #{7 3} #{1 3}},
            "7-1.3.5" #{#{7 1} #{7 5} #{7 3}},
            "1-3.5.7" #{#{7 1} #{1 5} #{1 3}}}))))
