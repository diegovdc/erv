(ns erv.utils.ratios-test
  (:require
   [cljs.test :refer [deftest is]]
   [erv.utils.parse :refer [exact->string parse-scale]]
   [erv.utils.ratios :refer [ratios->scale]]))

(deftest ratios->scale-test
  (is (= [{:ratio "1/1", :bounded-ratio "1/1", :bounding-period 2}
          {:ratio "5/4", :bounded-ratio "5/4", :bounding-period 2}
          {:ratio "3/2", :bounded-ratio "3/2", :bounding-period 2}]
         (->> (ratios->scale 2 (parse-scale "1 3 5/4"))
              (map #(-> %
                        (update :ratio exact->string)
                        (update :bounded-ratio exact->string)))))))
