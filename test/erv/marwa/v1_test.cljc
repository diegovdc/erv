(ns erv.marwa.v1-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.marwa.v1 :as marwa :refer [base-permutations get-possible-generator-sequences
                                   intervals->scale all-permutations]]))
;;;;;;;;
;; API
;;;;;;;;

(deftest get-possible-generator-sequences-test
  (is (= [{:generator 1,
           :sequence [2 2 1 2 2 2 1],
           :generator-freqs {2 5, 1 2},
           :best-sequence? false,
           :sorted-generators-by-high-freq [2 1]}
          {:generator 2,
           :sequence [4 3 4 3 3 4 3],
           :generator-freqs {4 3, 3 4},
           :best-sequence? false,
           :sorted-generators-by-high-freq [3 4]}
          {:generator 3,
           :sequence [5 6 5 5 5 5 5],
           :generator-freqs {5 6, 6 1},
           :best-sequence? true,
           :sorted-generators-by-high-freq [5 6]}
          {:generator 4,
           :sequence [7 7 7 7 7 6 7],
           :generator-freqs {7 6, 6 1},
           :best-sequence? true,
           :sorted-generators-by-high-freq [7 6]}
          {:generator 5,
           :sequence [9 8 9 9 8 9 8],
           :generator-freqs {9 4, 8 3},
           :best-sequence? false,
           :sorted-generators-by-high-freq [9 8]}
          {:generator 6,
           :sequence [11 10 10 10 11 10 10],
           :generator-freqs {11 2, 10 5},
           :best-sequence? false,
           :sorted-generators-by-high-freq [10 11]}]
         (get-possible-generator-sequences [2 2 1 2 2 2 1]))))

(deftest base-permutations-test
  (is (= [{:group-size 2, :generator-seq [8 6 7 7 7 7 6], :initial-index 0}
          {:group-size 4, :generator-seq [8 6 8 6 7 7 6], :initial-index 0}]
         (base-permutations 7 7 6))))

(deftest intervals->scale-test
  (is (= [2 2 1 2 2 2 1]
         (intervals->scale 12 [2 2 1 2 2 2 1])))
  (is (= [2 2 1 2 2 2 1]
         (intervals->scale 12 [7 7 7 7 7 6 7]))))

(deftest all-permutations-test
  (testing "Taking the base permutations of 12EDO diatonic, produce all permutations"
    (is (= [{:generator-seq [8 6 7 7 7 7 6],
             :group-size 2,
             :initial-index 0,
             :scale [2 2 2 2 1 2 1]}
            {:generator-seq [7 8 6 7 7 7 6],
             :group-size 2,
             :initial-index 1,
             :scale [3 1 2 1 2 2 1]}
            {:generator-seq [7 7 8 6 7 7 6],
             :group-size 2,
             :initial-index 2,
             :scale [2 2 2 1 3 1 1]}
            {:generator-seq [7 7 7 8 6 7 6],
             :group-size 2,
             :initial-index 3,
             :scale [2 3 1 1 2 2 1]}
            {:generator-seq [8 7 6 7 7 7 6],
             :group-size 1,
             :initial-index 2,
             :parent [8 6 7 7 7 7 6],
             :scale [3 1 2 2 1 2 1]}
            {:generator-seq [8 7 7 6 7 7 6],
             :group-size 1,
             :initial-index 3,
             :parent [8 6 7 7 7 7 6],
             :scale [3 1 2 2 2 1 1]}
            {:generator-seq [8 7 7 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [8 6 7 7 7 7 6],
             :scale [3 2 1 2 2 1 1]}
            {:generator-seq [7 8 7 6 7 7 6],
             :group-size 1,
             :initial-index 3,
             :parent [7 8 6 7 7 7 6],
             :scale [3 1 2 1 3 1 1]}
            {:generator-seq [7 8 7 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [7 8 6 7 7 7 6],
             :scale [3 2 1 1 3 1 1]}
            {:generator-seq [7 7 8 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [7 7 8 6 7 7 6],
             :scale [2 3 1 1 3 1 1]}
            {:generator-seq [8 6 8 6 7 7 6],
             :group-size 4,
             :initial-index 0,
             :scale [2 2 2 2 2 1 1]}
            {:generator-seq [7 8 6 8 6 7 6],
             :group-size 4,
             :initial-index 1,
             :scale [3 2 1 1 2 2 1]}
            {:generator-seq [8 7 6 8 6 7 6],
             :group-size 3,
             :initial-index 2,
             :parent [8 6 8 6 7 7 6],
             :scale [3 2 1 2 1 2 1]}
            {:generator-seq [8 6 7 8 6 7 6],
             :group-size 2,
             :initial-index 3,
             :parent [8 6 8 6 7 7 6],
             :scale [2 3 1 2 1 2 1]}
            {:generator-seq [8 6 8 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [8 6 8 6 7 7 6],
             :scale [2 3 1 2 2 1 1]}]
           (all-permutations 12 (base-permutations 7 7 6))))
    (testing "Only the scales, for quick manual check"
      (let [permutations (->> (base-permutations 7 7 6)
                              (all-permutations 12)
                              (map :scale))]
        (is (= '((2 2 2 2 1 2 1)
                 (3 1 2 1 2 2 1)
                 (2 2 2 1 3 1 1)
                 (2 3 1 1 2 2 1)
                 (3 1 2 2 1 2 1)
                 (3 1 2 2 2 1 1)
                 (3 2 1 2 2 1 1)
                 (3 1 2 1 3 1 1)
                 (3 2 1 1 3 1 1)
                 (2 3 1 1 3 1 1)
                 (2 2 2 2 2 1 1)
                 (3 2 1 1 2 2 1)
                 (3 2 1 2 1 2 1)
                 (2 3 1 2 1 2 1)
                 (2 3 1 2 2 1 1))
               permutations))
        (is (= 15 (count permutations)))))
    (testing "Only the generator chain, for quick manual check"
      (let [permutations (->> (base-permutations 7 7 6)
                              (all-permutations 12)
                              (map :generator-seq))]
        (is (= [[8 6 7 7 7 7 6]
                [7 8 6 7 7 7 6]
                [7 7 8 6 7 7 6]
                [7 7 7 8 6 7 6]
                [8 7 6 7 7 7 6]
                [8 7 7 6 7 7 6]
                [8 7 7 7 6 7 6]
                [7 8 7 6 7 7 6]
                [7 8 7 7 6 7 6]
                [7 7 8 7 6 7 6]
                [8 6 8 6 7 7 6]
                [7 8 6 8 6 7 6]
                [8 7 6 8 6 7 6]
                [8 6 7 8 6 7 6]
                [8 6 8 7 6 7 6]]
               permutations))))))

;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;

(deftest best-sequence?-test
  (is (true? (#'marwa/best-sequence? (frequencies '(7 7 7 7 7 6 7))))))

(deftest sequence-analysis-test
  (is (= {:generator-freqs {7 6, 6 1},
          :best-sequence? true,
          :sorted-generators-by-high-freq [7 6]}
         (#'marwa/sequence-analysis '(7 7 7 7 7 6 7)))))

(deftest interval-seq->degs-test
  (is (= [7 2 9 4 11 5 0]
         (#'marwa/interval-seq->degs 12 [7 7 7 7 7 6 7]))))

(deftest degs->scale-test
  (is (= [2 2 1 2 2 2 1]
         (#'marwa/degs->scale 12 [7 2 9 4 11 5 0]))))

(deftest get-generator-sequence-test
  (testing "Given a scale and and a range of intervals, produce the generator sequences of 12EDO diatonic")
  (is (= '((2 2 1 2 2 2 1)
           (4 3 4 3 3 4 3)
           (5 6 5 5 5 5 5)
           (7 7 7 7 7 6 7)
           (9 8 9 9 8 9 8)
           (11 10 10 10 11 10 10))
         (map (fn [i] (#'marwa/get-generator-sequence [2 2 1 2 2 2 1] i))
              (range 1 7)))))

(deftest permutate-range-fwd-test
  (is (= [7 8 6 7 7 7 6]
         (#'marwa/permutate-range-fwd 0 2 [8 6 7 7 7 7 6]))))

(deftest all-range-permutations-test
  (is (= [{:generator-seq [8 6 7 7 7 7 6], :group-size 2, :initial-index 0}
          {:generator-seq [7 8 6 7 7 7 6], :group-size 2, :initial-index 1}
          {:generator-seq [7 7 8 6 7 7 6], :group-size 2, :initial-index 2}
          {:generator-seq [7 7 7 8 6 7 6], :group-size 2, :initial-index 3}]
         (#'marwa/all-range-permutations [8 6 7 7 7 7 6] 2 0))))

(deftest all-sub-range-permutations-test
  (let [perms (#'marwa/all-sub-range-permutations [{:generator-seq [8 6 7 7 7 7 6], :group-size 2, :initial-index 0}])]
    (testing "Only the generator-seqs, for a quick check"
      (is (= [[8 6 7 7 7 7 6]
              [8 7 6 7 7 7 6]
              [8 7 7 6 7 7 6]
              [8 7 7 7 6 7 6]]
             (map :generator-seq perms))))
    (testing "the full data"
      (is (= [{:generator-seq [8 6 7 7 7 7 6],
               :group-size 1,
               :initial-index 1,
               :parent [8 6 7 7 7 7 6]}
              {:generator-seq [8 7 6 7 7 7 6],
               :group-size 1,
               :initial-index 2,
               :parent [8 6 7 7 7 7 6]}
              {:generator-seq [8 7 7 6 7 7 6],
               :group-size 1,
               :initial-index 3,
               :parent [8 6 7 7 7 7 6]}
              {:generator-seq [8 7 7 7 6 7 6],
               :group-size 1,
               :initial-index 4,
               :parent [8 6 7 7 7 7 6]}]
             perms))))
  (testing "The vector can contain more than  one generator chain data map"
    (is (= [{:generator-seq [8 6 7 7 7 7 6],
             :group-size 1,
             :initial-index 1,
             :parent [8 6 7 7 7 7 6]}
            {:generator-seq [8 7 6 7 7 7 6],
             :group-size 1,
             :initial-index 2,
             :parent [8 6 7 7 7 7 6]}
            {:generator-seq [8 7 7 6 7 7 6],
             :group-size 1,
             :initial-index 3,
             :parent [8 6 7 7 7 7 6]}
            {:generator-seq [8 7 7 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [8 6 7 7 7 7 6]}
            {:generator-seq [7 8 6 7 7 7 6],
             :group-size 1,
             :initial-index 2,
             :parent [7 8 6 7 7 7 6]}
            {:generator-seq [7 8 7 6 7 7 6],
             :group-size 1,
             :initial-index 3,
             :parent [7 8 6 7 7 7 6]}
            {:generator-seq [7 8 7 7 6 7 6],
             :group-size 1,
             :initial-index 4,
             :parent [7 8 6 7 7 7 6]}]
           (#'marwa/all-sub-range-permutations [{:generator-seq [8 6 7 7 7 7 6], :group-size 2, :initial-index 0}
                                                {:generator-seq [7 8 6 7 7 7 6], :group-size 2, :initial-index 1}])))))

(deftest all-permutations-for-base-permutation-test
  (is (= [[{:generator-seq [8 6 7 7 7 7 6], :group-size 2, :initial-index 0}
           {:generator-seq [7 8 6 7 7 7 6], :group-size 2, :initial-index 1}
           {:generator-seq [7 7 8 6 7 7 6], :group-size 2, :initial-index 2}
           {:generator-seq [7 7 7 8 6 7 6], :group-size 2, :initial-index 3}]
          [{:generator-seq [8 6 7 7 7 7 6],
            :group-size 1,
            :initial-index 1,
            :parent [8 6 7 7 7 7 6]}
           {:generator-seq [8 7 6 7 7 7 6],
            :group-size 1,
            :initial-index 2,
            :parent [8 6 7 7 7 7 6]}
           {:generator-seq [8 7 7 6 7 7 6],
            :group-size 1,
            :initial-index 3,
            :parent [8 6 7 7 7 7 6]}
           {:generator-seq [8 7 7 7 6 7 6],
            :group-size 1,
            :initial-index 4,
            :parent [8 6 7 7 7 7 6]}
           {:generator-seq [7 8 6 7 7 7 6],
            :group-size 1,
            :initial-index 2,
            :parent [7 8 6 7 7 7 6]}
           {:generator-seq [7 8 7 6 7 7 6],
            :group-size 1,
            :initial-index 3,
            :parent [7 8 6 7 7 7 6]}
           {:generator-seq [7 8 7 7 6 7 6],
            :group-size 1,
            :initial-index 4,
            :parent [7 8 6 7 7 7 6]}
           {:generator-seq [7 7 8 6 7 7 6],
            :group-size 1,
            :initial-index 3,
            :parent [7 7 8 6 7 7 6]}
           {:generator-seq [7 7 8 7 6 7 6],
            :group-size 1,
            :initial-index 4,
            :parent [7 7 8 6 7 7 6]}
           {:generator-seq [7 7 7 8 6 7 6],
            :group-size 1,
            :initial-index 4,
            :parent [7 7 7 8 6 7 6]}]]
         (#'marwa/all-permutations-for-base-permutation
          {:group-size 2, :generator-seq [8 6 7 7 7 7 6], :initial-index 0}))))

(deftest get-reciprocal-interval-test
  (is (= 8 (#'marwa/get-reciprocal-interval 7 6))))
