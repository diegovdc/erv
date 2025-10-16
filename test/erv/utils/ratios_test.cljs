(ns erv.utils.ratios-test
  (:require
   [cljs.test :refer [deftest is]]
   [erv.utils.exact :as exact.utils]
   [erv.utils.ratios :refer [gen-chain interval-seq->ratio-stack
                             normalize-ratios ratios->harmonic-series
                             ratios->scale ratios-intervals
                             seq-interval-analysis]]))

(deftest ratios->scale-test
  (is (= [{:ratio "1", :bounded-ratio "1", :bounding-period 2}
          {:ratio "5/4", :bounded-ratio "5/4", :bounding-period 2}
          {:ratio "3/2", :bounded-ratio "3/2", :bounding-period 2}]
         (->> "1 3 5/4"
              exact.utils/parse-ratios
              (ratios->scale 2)
              exact.utils/make-readable))))

(deftest ratios-intervals-test
  (is (= ["5/4" "6/5"]
         (->> "1 5/4 3/2"
              exact.utils/parse-ratios
              ratios-intervals
              exact.utils/make-readable))))

(deftest interval-seq->ratio-stack-test
  (is (= ["1" "3/2" "2" "3" "4" "6" "8"]
         (->> "3/2 4/3"
              exact.utils/parse-ratios
              (interval-seq->ratio-stack 7)
              exact.utils/make-readable))))

(deftest normalize-ratios-test
  (is (= ["1" "7/6" "3/2"]
         (->> (normalize-ratios [6 7 9])
              exact.utils/make-readable))))

(deftest ratios->harmonic-series-test
  (is (= ["6" "7" "9"]
         (->> "1 7/6 3/2"
              exact.utils/parse-ratios
              ratios->harmonic-series
              exact.utils/make-readable))))

(deftest seq-interval-analysis-test
  (is (= {:rooted-seq
          [["1" "1/1" 0]
           ["14/11" "2.7/11" 417.50796410436817]
           ["3/2" "3/2" 701.9550008653874]
           ["21/11" "3.7/11" 1119.4629649697556]
           ["2" "2/1" 1200]]
          :pairs
          [[["4/3" "56/33"] ["14/11" "2.7/11" 417.50796410436817]]
           [["56/33" "2"] ["33/28" "3.11/2.2.7" 284.44703676101926]]
           [["2" "28/11"] ["14/11" "2.7/11" 417.50796410436817]]
           [["28/11" "8/3"] ["22/21" "2.11/3.7" 80.53703503024445]]]
          :ratio-factorization
          [["4/3" "2.2/3"]
           ["56/33" "2.2.2.7/3.11"]
           ["2" "2/1"]
           ["28/11" "2.2.7/11"]
           ["8/3" "2.2.2/3"]]}
         (->> "4/3 56/33 2 28/11 8/3"
              exact.utils/parse-ratios
              seq-interval-analysis
              exact.utils/make-readable))))

(deftest gen-chain-test
  (is (= [1 3 9 27 81]
         (gen-chain 5 3))))
