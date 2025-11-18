(ns erv.lattice.v2-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [erv.utils.exact :as exact.utils]))

(deftest ratios->lattice-data-test
  (is (= '{:period "2",
           :min-x 0,
           :max-x 0,
           :min-y 0,
           :max-y 0,
           :data
           ({:ratio "3/2",
             :numerator "3",
             :denominator "2",
             :numer-factors ["3"],
             :denom-factors ["2"],
             :coords {:x 0, :y 0}}
            {:ratio "9/8",
             :numerator "9",
             :denominator "8",
             :numer-factors ["3" "3"],
             :denom-factors ["2" "2" "2"],
             :coords {:x 0, :y 0}}
            {:ratio "2",
             :numerator "2",
             :denominator "1",
             :numer-factors ["2"],
             :denom-factors [],
             :coords {:x 0, :y 0}}),
           :edges (({:x 0, :y 0} {:x 0, :y 0})
                   ({:x 0, :y 0} {:x 0, :y 0}))}

         (->> "3/2 9/8 2/1"
              exact.utils/parse-ratios
              (ratios->lattice-data base-coords)
              exact.utils/make-readable))))
