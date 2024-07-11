(ns erv.utils.scale-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.edo.core :as edo]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [degree-stack scale->stacked-subscale
                            scale-intervals tritriadic]]))

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
