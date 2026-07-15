(ns erv.meru.diagonals-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.math.pascals-triangle :as pascals-triangle]
   [erv.meru.diagonals :as subject]
   [erv.types :refer [MeruDiagonalsData]]
   [malli.core :as m]))

(deftest diagonals-test
  (testing "The Malli type is up to date"
    (let [data (subject/diagonals
                {:size 12
                 :slope {:x 1 :y 2}
                 :pascal-coord->number pascals-triangle/default-coord-map})]
      (is (m/validate MeruDiagonalsData data)
          (m/explain MeruDiagonalsData data))))
  (testing "Has a `:series` key"
    (is (= [1 1N 2N 3N 5N 8N 13N 21N 34N 55N 89N 144N]
           (->> (subject/diagonals
                 {:size 12
                  :slope {:x 1 :y 2}
                  :pascal-coord->number pascals-triangle/default-coord-map})
                :series))))
  (testing "Can work with a custom `pascal-coord->number function (or map)"
    (is (= [1 1N 2N 3N 5N 8N 13N 21N 34N 55N 89N 144N]
           (->> (subject/diagonals
                 {:size 12
                  :slope {:x 1 :y 2}
                  :pascal-coord->number (pascals-triangle/make-coord-map 1 1 30)})
                :series))))
  (testing "Has a `:convergence-double` and a `:convergence-double-with-precision` key"
    (is (= [1.617977528089888 1.618]
           (->> (subject/diagonals
                 {:size 12
                  :slope {:x 1 :y 2}
                  :convergence-precision 3
                  :pascal-coord->number pascals-triangle/default-coord-map})
                ((juxt :convergence-double :convergence-double-with-precision))))))
  (testing "May have a `:triangle-seed` key, specially if `:pascal-coord->number` is `pascals-triangle/default-coord-map` or was created with `pascals-triangle/make-coord-map`."
    (is (= {:left 1, :right 1}
           (->> (subject/diagonals
                 {:size 12
                  :slope {:x 1 :y 2}
                  :convergence-precision 3
                  :pascal-coord->number pascals-triangle/default-coord-map})
                :triangle-seed)
           (->> (subject/diagonals
                 {:size 12
                  :slope {:x 1 :y 2}
                  :convergence-precision 3
                  :pascal-coord->number (pascals-triangle/make-coord-map 1 1 30)})
                :triangle-seed)))))
