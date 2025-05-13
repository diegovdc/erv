(ns erv.utils.scale-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.edo.core :as edo]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [cross-set dedupe-scale degree-stack diamond
                            find-subset-degrees get-degrees rotate-scale
                            scale->stacked-subscale scale-intervals
                            scale-steps->degrees tritriadic]]))

(deftest degree-stack-test
  (is (= [0 4 8]
         (map :edo/degree
              (degree-stack {:scale (:scale (edo/from-pattern (repeat 12 1)))
                             :gen 4
                             :offset 0}))))
  (testing "Will make an ordered list with the degrees from the stack"
    (is (= [0 7 2 9 4 11 6 1 8 3 10 5]
           (map :edo/degree
                (degree-stack {:scale (:scale (edo/from-pattern (repeat 12 1)))
                               :gen 7
                               :offset 0}))))))

(deftest scale-intervals-test
  (is (= [5/4 6/5 4/3]
         (scale-intervals (ratios->scale [1 5/4 3/2])))))

(deftest tritriadic-test
  (testing "Can make a tritriadic scale (Tritriad69)"
    (is (= {:meta {:scale :tritriadic
                   :triad-ratios [1 7/6 3/2]}
            :scale [{:ratio 9/8 :bounded-ratio 9/8 :bounding-period 2}
                    {:ratio 81/64 :bounded-ratio 81/64 :bounding-period 2}
                    {:ratio 21/16 :bounded-ratio 21/16 :bounding-period 2}
                    {:ratio 3/2 :bounded-ratio 3/2 :bounding-period 2}
                    {:ratio 27/16 :bounded-ratio 27/16 :bounding-period 2}
                    {:ratio 7/4 :bounded-ratio 7/4 :bounding-period 2}
                    {:ratio 63/32 :bounded-ratio 63/32 :bounding-period 2}]}
           (tritriadic [1 7/6 3/2]))))
  (testing "Can take a series of harmonics (integers)"
    (is (= {:meta {:scale :tritriadic
                   :triad-ratios [1 7/6 3/2]}
            :scale [{:ratio 9/8 :bounded-ratio 9/8 :bounding-period 2}
                    {:ratio 81/64 :bounded-ratio 81/64 :bounding-period 2}
                    {:ratio 21/16 :bounded-ratio 21/16 :bounding-period 2}
                    {:ratio 3/2 :bounded-ratio 3/2 :bounding-period 2}
                    {:ratio 27/16 :bounded-ratio 27/16 :bounding-period 2}
                    {:ratio 7/4 :bounded-ratio 7/4 :bounding-period 2}
                    {:ratio 63/32 :bounded-ratio 63/32 :bounding-period 2}]}
           (tritriadic [6 7 9])))))

(deftest scale->stacked-subscale-test
  (is (= {:meta
          {:scale :stacked-subscale
           :intervals [3/2 4/3 3/2 4/3],
           :parent-scale
           [{:ratio 1, :bounded-ratio 1, :bounding-period 4}
            {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 4}
            {:ratio 2N, :bounded-ratio 2N, :bounding-period 4}
            {:ratio 3N, :bounded-ratio 3N, :bounding-period 4}],
           :gen 2,
           :starting-offset 0},
          :scale
          [{:ratio 1, :bounded-ratio 1, :bounding-period 4}
           {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 4}
           {:ratio 2N, :bounded-ratio 2N, :bounding-period 4}
           {:ratio 3N, :bounded-ratio 3N, :bounding-period 4}]}

         (scale->stacked-subscale {:scale (ratios->scale [1 5/4 3/2 15/8])
                                   :gen 2
                                   :offset 0
                                   :size 40
                                   :period 4})))
  (is (= [1 9/8 3/2]
         (->> (scale->stacked-subscale {:scale (ratios->scale 3 [1 5/4 3/2 15/8])
                                        :gen 2
                                        :offset 0
                                        :size 4})
              :scale
              (map :bounded-ratio)))))

(deftest dedupe-scale-test
  (is (= [{:bounded-ratio 1}
          {:bounded-ratio 5/4}
          {:bounded-ratio 3/2}]
         (dedupe-scale [{:bounded-ratio 1}
                        {:bounded-ratio 5/4}
                        {:bounded-ratio 3/2}
                        {:bounded-ratio 3/2}]))))

(deftest rotate-scale-test
  (testing "Rotates the scale and leaves a trace of the original degree and ratio"
    (is (= [{:ratio 1,
             :bounded-ratio 1,
             :bounding-period 2,
             :rotated-scale/original-degree 1,
             :rotated-scale/original-ratio 5/4}
            {:ratio 6/5,
             :bounded-ratio 6/5,
             :bounding-period 2,
             :rotated-scale/original-degree 2,
             :rotated-scale/original-ratio 3/2}
            {:ratio 8/5,
             :bounded-ratio 8/5,
             :bounding-period 2,
             :rotated-scale/original-degree 0,
             :rotated-scale/original-ratio 1}]
           (rotate-scale 1 (ratios->scale [1 5/4 3/2])))))
  ;; TODO
  #_(testing "Rotating twice is the same as rotating by the sum of the rotation steps"
      (is (= (rotate-scale 2 (ratios->scale [1 5/4 3/2]))
             (->> (ratios->scale [1 5/4 3/2])
                  (rotate-scale 1)
                  (rotate-scale 1)))))
  (testing "Preserves other note data"
    (is (= [{:ratio 1,
             :bounded-ratio 1,
             :bounding-period 2,
             :some-data "hola",
             :rotated-scale/original-degree 1,
             :rotated-scale/original-ratio 5/4}
            {:ratio 6/5,
             :bounded-ratio 6/5,
             :bounding-period 2,
             :some-data "hola",
             :rotated-scale/original-degree 2,
             :rotated-scale/original-ratio 3/2}
            {:ratio 8/5,
             :bounded-ratio 8/5,
             :bounding-period 2,
             :some-data "hola",
             :rotated-scale/original-degree 0,
             :rotated-scale/original-ratio 1}]
           (rotate-scale 1
                         (->> (ratios->scale [1 5/4 3/2])
                              (map #(assoc % :some-data "hola"))))))))

(deftest cross-set-test
  (is (= {:meta
          {:scale :cross-set,
           :sets [[1 3 9] [1 5 25] [1 7 49]],
           :size 27,
           :period 2},
          :scale
          [{:ratio 1, :bounded-ratio 1, :bounding-period 2}
           {:ratio 525/512, :bounded-ratio 525/512, :bounding-period 2}
           {:ratio 2205/2048, :bounded-ratio 2205/2048, :bounding-period 2}
           {:ratio 35/32, :bounded-ratio 35/32, :bounding-period 2}
           {:ratio 9/8, :bounded-ratio 9/8, :bounding-period 2}
           {:ratio 147/128, :bounded-ratio 147/128, :bounding-period 2}
           {:ratio 75/64, :bounded-ratio 75/64, :bounding-period 2}
           {:ratio 1225/1024, :bounded-ratio 1225/1024, :bounding-period 2}
           {:ratio 315/256, :bounded-ratio 315/256, :bounding-period 2}
           {:ratio 5/4, :bounded-ratio 5/4, :bounding-period 2}
           {:ratio 21/16, :bounded-ratio 21/16, :bounding-period 2}
           {:ratio 11025/8192, :bounded-ratio 11025/8192, :bounding-period 2}
           {:ratio 175/128, :bounded-ratio 175/128, :bounding-period 2}
           {:ratio 45/32, :bounded-ratio 45/32, :bounding-period 2}
           {:ratio 735/512, :bounded-ratio 735/512, :bounding-period 2}
           {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 2}
           {:ratio 49/32, :bounded-ratio 49/32, :bounding-period 2}
           {:ratio 1575/1024, :bounded-ratio 1575/1024, :bounding-period 2}
           {:ratio 25/16, :bounded-ratio 25/16, :bounding-period 2}
           {:ratio 105/64, :bounded-ratio 105/64, :bounding-period 2}
           {:ratio 441/256, :bounded-ratio 441/256, :bounding-period 2}
           {:ratio 7/4, :bounded-ratio 7/4, :bounding-period 2}
           {:ratio 225/128, :bounded-ratio 225/128, :bounding-period 2}
           {:ratio 3675/2048, :bounded-ratio 3675/2048, :bounding-period 2}
           {:ratio 15/8, :bounded-ratio 15/8, :bounding-period 2}
           {:ratio 245/128, :bounded-ratio 245/128, :bounding-period 2}
           {:ratio 63/32, :bounded-ratio 63/32, :bounding-period 2}]}
         (cross-set 2 [1 3 9] [1 5 25] [1 7 49]))))

(deftest find-subset-degrees-test
  (is (= '({:degrees (0 2 4), :matched 1, :subscale/matched-ratios (1 5/4 3/2)}
           {:degrees (3 5 0), :matched 1, :subscale/matched-ratios (1 5/4 3/2)}
           {:degrees (4 6 1), :matched 1, :subscale/matched-ratios (1 5/4 3/2)})
         (find-subset-degrees
          {:scale (ratios->scale [1 9/8 5/4 4/3 3/2 5/3 15/8])
           :subset-ratios [1 5/4 3/2]})))
  (testing "Can accept partial matches using `:max-missing-notes`"
    (is (= '({:degrees (0 2 4), :matched 1, :subscale/matched-ratios (1 5/4 3/2)}
             {:degrees (2 6), :matched 2/3, :subscale/matched-ratios (1 3/2)}
             {:degrees (3 5 0), :matched 1, :subscale/matched-ratios (1 5/4 3/2)}
             {:degrees (4 6 1), :matched 1, :subscale/matched-ratios (1 5/4 3/2)}
             {:degrees (5 2), :matched 2/3, :subscale/matched-ratios (1 3/2)})
           (find-subset-degrees
            {:scale (ratios->scale [1 9/8 5/4 4/3 3/2 5/3 15/8])
             :subset-ratios [1 5/4 3/2]
             :max-missing-notes 1})))))

(deftest get-degrees-test
  (is (= [{:bounding-period 2, :bounded-ratio 1, :ratio 1}
          {:bounding-period 2, :bounded-ratio 5/4, :ratio 5/4}
          {:bounding-period 2, :bounded-ratio 3/2, :ratio 3/2}]
         (get-degrees
          (ratios->scale [1 9/8 5/4 4/3 3/2 5/3 15/8])
          [0 2 4])))
  (testing "If the degree is outside the bounds of the scale it will wrap"
    (is (= [{:bounding-period 2, :bounded-ratio 1, :ratio 1}
            {:bounding-period 2, :bounded-ratio 5/4, :ratio 5/4}
            {:bounding-period 2, :bounded-ratio 3/2, :ratio 3/2}]
           (get-degrees
            (ratios->scale [1 9/8 5/4 4/3 3/2 5/3 15/8])
            [0 2 11])))))

(deftest scale-steps->degrees-test
  (is (= [0 2 4 5 7 9 11 12]
         (scale-steps->degrees [2 2 1 2 2 2 1] false))))

(deftest diamond-test
  (is (= {:meta {:scale :diamond
                 :factors [1 3 5 7 9]
                 :period 2
                 :size 19}
          :scale
          [{:bounded-ratio 1 :bounding-period 2 :ratio 1}
           {:bounded-ratio 10/9 :bounding-period 2 :ratio 10/9}
           {:bounded-ratio 9/8 :bounding-period 2 :ratio 9/8}
           {:bounded-ratio 8/7 :bounding-period 2 :ratio 8/7}
           {:bounded-ratio 7/6 :bounding-period 2 :ratio 7/6}
           {:bounded-ratio 6/5 :bounding-period 2 :ratio 6/5}
           {:bounded-ratio 5/4 :bounding-period 2 :ratio 5/4}
           {:bounded-ratio 9/7 :bounding-period 2 :ratio 9/7}
           {:bounded-ratio 4/3 :bounding-period 2 :ratio 4/3}
           {:bounded-ratio 7/5 :bounding-period 2 :ratio 7/5}
           {:bounded-ratio 10/7 :bounding-period 2 :ratio 10/7}
           {:bounded-ratio 3/2 :bounding-period 2 :ratio 3/2}
           {:bounded-ratio 14/9 :bounding-period 2 :ratio 14/9}
           {:bounded-ratio 8/5 :bounding-period 2 :ratio 8/5}
           {:bounded-ratio 5/3 :bounding-period 2 :ratio 5/3}
           {:bounded-ratio 12/7 :bounding-period 2 :ratio 12/7}
           {:bounded-ratio 7/4 :bounding-period 2 :ratio 7/4}
           {:bounded-ratio 16/9 :bounding-period 2 :ratio 16/9}
           {:bounded-ratio 9/5 :bounding-period 2 :ratio 9/5}]}
         (diamond 2 1 3 5 7 9)))

  (testing "A `diamond` is a special case of `cross-set`"
    (is (=
         (map :bounded-ratio (:scale (diamond 2 1 3 5 7 9)))
         (map :bounded-ratio (:scale (cross-set 2
                                                [1 3 5 7 9]
                                                (map #(/ 1 %) [1 3 5 7 9]))))))))
