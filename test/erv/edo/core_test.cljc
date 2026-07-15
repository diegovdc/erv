(ns erv.edo.core-test
  (:require
   [clojure.math :refer [round]]
   [clojure.test :refer [deftest is]]
   [erv.edo.core :refer [from-pattern]]
   [erv.utils.conversions :refer [ratio->cents]]))

(deftest from-pattern-test
  (is (= #?(:clj
            {:meta {:edo/pattern [2 2 1 2 2 2 1]
                    :edo/divisions 12
                    :edo/period 2},
             :scale
             [{:bounded-ratio 1.0,
               :bounding-period 2,
               :edo/degree 0,
               :edo/original-degree 0}
              {:bounded-ratio 1.122462048309373,
               :bounding-period 2,
               :edo/degree 1,
               :edo/original-degree 2}
              {:bounded-ratio 1.2599210498948732,
               :bounding-period 2,
               :edo/degree 2,
               :edo/original-degree 4}
              {:bounded-ratio 1.3348398541700344,
               :bounding-period 2,
               :edo/degree 3,
               :edo/original-degree 5}
              {:bounded-ratio 1.4983070768766813,
               :bounding-period 2,
               :edo/degree 4,
               :edo/original-degree 7}
              {:bounded-ratio 1.6817928305074292,
               :bounding-period 2,
               :edo/degree 5,
               :edo/original-degree 9}
              {:bounded-ratio 1.887748625363387,
               :bounding-period 2,
               :edo/degree 6,
               :edo/original-degree 11}]}
            ;;NOTE slightly different value on cljs
            :cljs
            {:meta {:edo/pattern [2 2 1 2 2 2 1]
                    :edo/divisions 12
                    :edo/period 2}
             :scale [{:edo/original-degree 0
                      :edo/degree 0
                      :bounded-ratio 1
                      :bounding-period 2}
                     {:edo/original-degree 2
                      :edo/degree 1
                      :bounded-ratio 1.1224620483093728
                      :bounding-period 2}
                     {:edo/original-degree 4
                      :edo/degree 2
                      :bounded-ratio 1.2599210498948732
                      :bounding-period 2}
                     {:edo/original-degree 5
                      :edo/degree 3
                      :bounded-ratio 1.3348398541700344
                      :bounding-period 2}
                     {:edo/original-degree 7
                      :edo/degree 4
                      :bounded-ratio 1.4983070768766815
                      :bounding-period 2}
                     {:edo/original-degree 9
                      :edo/degree 5
                      :bounded-ratio 1.6817928305074292
                      :bounding-period 2}
                     {:edo/original-degree 11
                      :edo/degree 6
                      :bounded-ratio 1.8877486253633868
                      :bounding-period 2}]})

         (from-pattern [2 2 1 2 2 2 1])))
  (is (= [0 200 400 500 700 900 1100]
         (->> (from-pattern [2 2 1 2 2 2 1])
              :scale
              (map (comp round ratio->cents :bounded-ratio))))))
