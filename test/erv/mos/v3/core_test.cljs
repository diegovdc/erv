(ns erv.mos.v3.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.mos.v3.core :refer [gen->mos-ratios]]
   [erv.utils.exact :as exact.utils]))

(deftest gen->mos-ratios-test
  (testing "Basic test"
    (let [result (-> (gen->mos-ratios (exact.utils/parse-ratio "3/2") 2 7)
                     (exact.utils/make-readable))]
      (is (= '{:meta
               {:mos/pattern.name "2s5L",
                :mos/normalized-by 1,
                :mos/generator "3/2",
                :scale :mos,
                :mos/s.cents 90.22499567306306,
                :mos/L.cents 203.91000173077484,
                :mos/pattern "LLLsLLs",
                :intervals/cents
                (203.91000173077484
                 203.91000173077484
                 203.91000173077484
                 90.22499567306306
                 203.91000173077484
                 203.91000173077484
                 90.22499567306306),
                :size 7,
                :intervals/ratios ("9/8" "9/8" "9/8" "256/243" "9/8" "9/8" "256/243"),
                :mos/s "256/243",
                :mos/L "9/8",
                :mos/sL-ratio.float 2.2600167526708206,
                :period "2",
                :mos/sL-ratio.cents 1411.600160215743,
                :mos/type :ratio,
                :mos/sL-ratio "2187/2048"},
               :scale
               ({:ratio "1", :bounded-ratio "1", :bounding-period "2"}
                {:ratio "9/8", :bounded-ratio "9/8", :bounding-period "2"}
                {:ratio "81/64", :bounded-ratio "81/64", :bounding-period "2"}
                {:ratio "729/512", :bounded-ratio "729/512", :bounding-period "2"}
                {:ratio "3/2", :bounded-ratio "3/2", :bounding-period "2"}
                {:ratio "27/16", :bounded-ratio "27/16", :bounding-period "2"}
                {:ratio "243/128", :bounded-ratio "243/128", :bounding-period "2"})}
             (last result)))))
  (testing "Integer generator"
    (let [result (-> (gen->mos-ratios 3 2 7)
                     (exact.utils/make-readable))]
      (is (= '{:meta
               {:mos/pattern.name "2s5L",
                :mos/normalized-by 1,
                :mos/generator 3,
                :scale :mos,
                :mos/s.cents 90.22499567306306,
                :mos/L.cents 203.91000173077484,
                :mos/pattern "LLLsLLs",
                :intervals/cents
                (203.91000173077484
                 203.91000173077484
                 203.91000173077484
                 90.22499567306306
                 203.91000173077484
                 203.91000173077484
                 90.22499567306306),
                :size 7,
                :intervals/ratios ("9/8" "9/8" "9/8" "256/243" "9/8" "9/8" "256/243"),
                :mos/s "256/243",
                :mos/L "9/8",
                :mos/sL-ratio.float 2.2600167526708206,
                :period "2",
                :mos/sL-ratio.cents 1411.600160215743,
                :mos/type :ratio,
                :mos/sL-ratio "2187/2048"},
               :scale
               ({:ratio "1", :bounded-ratio "1", :bounding-period "2"}
                {:ratio "9/8", :bounded-ratio "9/8", :bounding-period "2"}
                {:ratio "81/64", :bounded-ratio "81/64", :bounding-period "2"}
                {:ratio "729/512", :bounded-ratio "729/512", :bounding-period "2"}
                {:ratio "3/2", :bounded-ratio "3/2", :bounding-period "2"}
                {:ratio "27/16", :bounded-ratio "27/16", :bounding-period "2"}
                {:ratio "243/128", :bounded-ratio "243/128", :bounding-period "2"})}
             (last result)))))
  (testing "Floating point generator"
    (let [result (-> (gen->mos-ratios 1.618 2 7)
                     (exact.utils/make-readable))]
      (is (= '{:meta
               {:mos/pattern.name "4s3L",
                :mos/normalized-by 1,
                :mos/generator 1.618,
                :scale :mos,
                :mos/s.cents 99.16178796440605,
                :mos/L.cents 267.78428271412537,
                :mos/pattern "ssLsLsL",
                :intervals/cents
                (99.16178796440605
                 99.16178796440605
                 267.78428271412537
                 99.16178796440605
                 267.78428271412537
                 99.16178796440605
                 267.78428271412537),
                :size 7,
                :intervals/ratios
                ("529475129/500000000"
                 "529475129/500000000"
                 "500000000000/428345379361"
                 "529475129/500000000"
                 "500000000000/428345379361"
                 "529475129/500000000"
                 "500000000000/428345379361"),
                :mos/s "529475129/500000000",
                :mos/L "500000000000/428345379361",
                :mos/sL-ratio.float 2.700478563478969,
                :period "2",
                :mos/sL-ratio.cents 1719.8581153882103,
                :mos/type :ratio,
                :mos/sL-ratio "250000000000000000000/226798224993719412569"},
               :scale
               ({:ratio "1", :bounded-ratio "1", :bounding-period "2"}
                {:ratio "529475129/500000000",
                 :bounded-ratio "529475129/500000000",
                 :bounding-period "2"}
                {:ratio "280343912229566641/250000000000000000",
                 :bounded-ratio "280343912229566641/250000000000000000",
                 :bounding-period "2"}
                {:ratio "654481/500000",
                 :bounded-ratio "654481/500000",
                 :bounding-period "2"}
                {:ratio "346531411903049/250000000000000",
                 :bounded-ratio "346531411903049/250000000000000",
                 :bounding-period "2"}
                {:ratio "809/500", :bounded-ratio "809/500", :bounding-period "2"}
                {:ratio "428345379361/250000000000",
                 :bounded-ratio "428345379361/250000000000",
                 :bounding-period "2"})}
             (last result))))))
