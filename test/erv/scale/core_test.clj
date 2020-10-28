(ns erv.scale.core-test
  (:require
   [erv.scale.core :refer [deg->freq
                           intervals->degs
                           stateful-interval->degree
                           demo-scale*]]
   [erv.cps.core :as cps]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest testing is]]))

;; Setup
(def hexany
  (->> [6 7 5 9]
       (cps/->cps 2)
       cps/set->maps
       (cps/bound-ratio 2)
       (cps/maps->data :bounded-ratio)))


(deftest cps-scale-fulfills-the-scale-spec
  (is (true? (s/valid? :erv.scale.core/scale (hexany :scale)))))

(deftest deg->freq-test
  (testing "octaves up"
    (is (= '(35/32 35/16 35/8)
           (->> [0 6 12] (map #(deg->freq (hexany :scale) 1 %)))))
    (is (= '(350/32 350/16 350/8)
           (->> [0 6 12] (map #(deg->freq (hexany :scale) 10 %)))))
    (is (= '(35/32 21/16 21/8)
           (->> [0 1 7] (map #(deg->freq (hexany :scale) 1 %))))))
  (testing "octaves down"
    (is (= '(35/128 35/32 35/16)
           (->> [-12 0 6] (map #(deg->freq (hexany :scale) 1 %)))))
    (is (= '(35/512 35/128 35/32)
           (->> [-24 -12 0] (map #(deg->freq (hexany :scale) 1 %))))))
  (testing "period"
    (is (= '(35/16 35/8 35/4)
           (->> [0 6 12] (map #(deg->freq (hexany :scale) 1 % :period 1)))))))

(deftest intervals->degs-test
  (testing "upward sequence"
    (is (= '[0 1 3 6 10 15]
           (intervals->degs 0 [1 2 3 4 5]))))
  (testing "downward sequence"
    (is (= '[0 -1 -3 -6 -10 -15]
           (intervals->degs 0 [-1 -2 -3 -4 -5]))))
  (testing "mixed sequence"
    (is (= '[0 -1 1 -2 2 -3]
           (intervals->degs 0 [-1 2 -3 4 -5]))))
  (testing "starting on a different base-deg"
    (is (= '[1 0 2 -1 3 -2]
           (intervals->degs 1 [-1 2 -3 4 -5])))))

(deftest stateful-interval->degree-test
  (let [i->d (stateful-interval->degree)
        i->d2 (stateful-interval->degree)]
    (testing "statefulness of the returned function"
      (is (= 5 (i->d 5)))
      (is (= 10 (i->d 5)))
      (is (= 7 (i->d -3))))
    (testing "independence of the functions"
      (is (and (= 4 (i->d -3))
               (= -3 (i->d2 -3)))))
    (testing "initial degree"
      (let [i->d3 (stateful-interval->degree 5)]
        (is (= 5 (i->d3 0)))))
    (testing "can be mapped"
      (let [i->d4 (stateful-interval->degree)]
        (= [1 3 6 10]
           (mapv i->d4 [1 2 3 4]))))))


(deftest demo-scale*-test
  (testing "single period, default base-freq @ 440hz"
    (is (= '(1925/4 1155/2 2475/4 1485/2 825N 3465/4 1925/2 3465/4 825N 1485/2 2475/4 1155/2 1925/4)
           (demo-scale* (hexany :scale) 1))))
  (testing "single period, custom base-freq @ 1hz"
    (is (= '(35/32 21/16 45/32 27/16 15/8 63/32 35/16 63/32 15/8 27/16 45/32 21/16 35/32)
           (demo-scale* (hexany :scale) 1 1))))
  (testing "two periods"
    (is (= '(35/32 21/16 45/32 27/16 15/8 63/32 35/16 21/8 45/16 27/8 15/4 63/16 35/8 63/16 15/4 27/8 45/16 21/8 35/16 63/32 15/8 27/16 45/32 21/16 35/32)
           (demo-scale* (hexany :scale) 2 1)))))
