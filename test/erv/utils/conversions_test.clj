(ns erv.utils.conversions-test
  (:require
   [clojure.test :refer [deftest is]]
   [erv.utils.conversions :as subject]))

(deftest ratio->cents-test
  (is (= 701.9550008653874
         (subject/ratio->cents 3/2))))
