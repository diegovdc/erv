(ns erv.beating-analyzer.v1-test
  (:require
   [cljs.pprint :as pprint]
   [clojure.test :refer [deftest is]]
   [erv.beating-analyzer.v1 :as subject]
   [erv.utils.exact :as exact.utils]))

(deftest get-beat-data-test
  (let [data (subject/get-beat-data
              (exact.utils/->exact 2)
              (exact.utils/->exact 1)
              (map exact.utils/->exact (range 1 6))
              (exact.utils/parse-ratios "1 5/4 3/2"))]
    (is (= [{:root-hz 32,
             :degree-1 0,
             :ratio-2-partial "1",
             :degree-2 1,
             :beat-freq.ratio "8",
             :beat-freq.factors {:numer ["2" "2" "2"], :denom []},
             :root "B0+62",
             :ratio-1 "1",
             :ratio-1-partial "1",
             :period 0,
             :ratio-2 "5/4",
             :beat-freq.hz 8}]
           (->> data
                (take 1)
                (exact.utils/make-readable))))
    (is (= 30 (count data)))))

#_(pprint/pprint
   (first
    (exact.utils/make-readable
     (subject/get-beat-data
      (exact.utils/->exact 2)
      (exact.utils/->exact 1)
      (map exact.utils/->exact (range 1 6))
      (exact.utils/parse-ratios "1 5/4 3/2")))))
