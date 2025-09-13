(ns erv.meru.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.meru.core :as subject]))

(deftest convergence-mos-data-summary-test
  (is (= [{:size 2,
           :mos/pattern.name "1s1L",
           :mos/sL-ratio.float (float 2.2702353),
           :mos/s.cents 366.9460706785318,
           :mos/L.cents 833.0539293214687}
          {:size 3,
           :mos/pattern.name "2s1L",
           :mos/sL-ratio.float (float 1.2702353),
           :mos/s.cents 366.9460706785318,
           :mos/L.cents 466.10785864293723}
          {:size 4,
           :mos/pattern.name "1s3L",
           :mos/sL-ratio.float (float 3.7004786),
           :mos/s.cents 99.16178796440605,
           :mos/L.cents 366.9460706785318}]
         (subject/convergence-mos-data-summary
          {:max-size 4
           :period 2}
          (subject/diagonals {:size 20
                              :slope {:x 1 :y 2}})))))
