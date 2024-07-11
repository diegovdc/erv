(ns erv.utils.ratios-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.ratios :refer [interval-seq->ratio-stack normalize-ratios
                             ratios->harmonic-series ratios->scale ratios-intervals]]))

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
