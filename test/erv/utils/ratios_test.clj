(ns erv.utils.ratios-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.ratios :refer [gen-chain interval-seq->ratio-stack
                             normalize-ratios ratios->harmonic-series
                             ratios->scale ratios-intervals
                             seq-interval-analysis]]))

(deftest ratios->scale-test
  (is (= [{:ratio 1, :bounded-ratio 1, :bounding-period 2}
          {:ratio 5/4, :bounded-ratio 5/4, :bounding-period 2}
          {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 2}]
         (ratios->scale 2 [1 3 5/4]))))

(deftest ratios-intervals-test
  (is (= [5/4 6/5]
         (ratios-intervals [1 5/4 3/2]))))

(deftest interval-seq->ratio-stack-test
  (is (= [1 3/2 2N 3N 4N 6N 8N]
         (interval-seq->ratio-stack [3/2 4/3] 7))))

(deftest normalize-ratios-test
  (is (=  [1 7/6 3/2]
          (normalize-ratios [6 7 9])))
  (is (=  [1 7/6 3]
          (normalize-ratios [6 7 18])))
  (is (=  [1 7/6 3/2]
          (normalize-ratios 2 [6 7 18]))))

(deftest ratios->harmonic-series-test
  (is (= [6 7 9]
         (ratios->harmonic-series [1/1 7/6 3/2]))))

(deftest seq-interval-analysis-test
  (is (= {:rooted-seq
          [[1N "1/1" 0.0]
           [14/11 "2.7/11" 417.5079641043685]
           [3/2 "3/2" 701.9550008653874]
           [21/11 "3.7/11" 1119.4629649697556]
           [2N "2/1" 1200.0]]
          :pairs
          [[[4/3 56/33] [14/11 "2.7/11" 417.5079641043685]]
           [[56/33 2] [33/28 "3.11/2.2.7" 284.4470367610199]]
           [[2 28/11] [14/11 "2.7/11" 417.5079641043685]]
           [[28/11 8/3] [22/21 "2.11/3.7" 80.53703503024482]]]
          :ratio-factorization
          [[4/3 "2.2/3"]
           [56/33 "2.2.2.7/3.11"]
           [2 "2/1"]
           [28/11 "2.2.7/11"]
           [8/3 "2.2.2/3"]]}
         (seq-interval-analysis [4/3 56/33 2/1 28/11 8/3]))))

(deftest gen-chain-test
  (is (= [1 3 9 27 81]
         (gen-chain 5 3))))
