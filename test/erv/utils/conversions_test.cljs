(ns erv.utils.conversions-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.conversions :as subject]
   [erv.utils.exact :as exact.utils]))

(deftest ratio->cents-test
  (is (= 701.9550008653874
         (subject/ratio->cents (exact.utils/parse-ratio "3/2")))))
