(ns erv.types
  (:require [malli.core :as m]
            [malli.util :as mu]))

(def Intish
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  [:or :int [:fn #(instance? clojure.lang.BigInt %)]])

(do
  (def MeruBaseData
    [:map
     [:convergence-double double?]
     [:convergence-index int?]
     [:reached-convergence? :boolean]
     [:series [:vector Intish]]])

  (def MeruDiagonalsData
    (mu/merge MeruBaseData
              [:map
               [:series-data [:vector
                              [:map
                               [:value Intish]
                               [:coords [:vector
                                         [:map
                                          [:x :int]
                                          [:y Intish]]]]
                               [:ratio-vs-previous {:optional true} [:maybe :double]]]]]
               [:triangle-seed [:map [:left :int] [:right :int]]]
               [:convergence-precision :int]
               [:convergence-ratio-with-precision :double]]))

  (m/explain MeruDiagonalsData
             {:convergence-double 2.0,
              :convergence-index 18,
              :series-data
              [{:value 1, :coords [{:x 0, :y 0N}]}
               {:value 0, :coords [], :ratio-vs-previous 0.0}
               {:value 0, :coords [], :ratio-vs-previous nil}
               {:value 1N, :coords [{:x 0, :y 1N}], :ratio-vs-previous nil}
               {:value 0, :coords [], :ratio-vs-previous 0.0}
               {:value 1N, :coords [{:x 1, :y 0N}], :ratio-vs-previous nil}
               {:value 1N, :coords [{:x 0, :y 2N}], :ratio-vs-previous 1.0}
               {:value 0, :coords [], :ratio-vs-previous 0.0}
               {:value 2N, :coords [{:x 1, :y 1N}], :ratio-vs-previous nil}
               {:value 1N, :coords [{:x 0, :y 3N}], :ratio-vs-previous 0.5}
               {:value 1N, :coords [{:x 2, :y 0N}], :ratio-vs-previous 1.0}
               {:value 3N, :coords [{:x 1, :y 2N}], :ratio-vs-previous 3.0}
               {:value 1N, :coords [{:x 0, :y 4N}], :ratio-vs-previous 0.3333333333333333}
               {:value 3N, :coords [{:x 2, :y 1N}], :ratio-vs-previous 3.0}
               {:value 4N, :coords [{:x 1, :y 3N}], :ratio-vs-previous 1.333333333333333}
               {:value 2N, :coords [{:x 0, :y 5N} {:x 3, :y 0N}], :ratio-vs-previous 0.5}
               {:value 6N, :coords [{:x 2, :y 2N}], :ratio-vs-previous 3.0}
               {:value 5N, :coords [{:x 1, :y 4N}], :ratio-vs-previous 0.8333333333333333}
               {:value 5N, :coords [{:x 0, :y 6N} {:x 3, :y 1N}], :ratio-vs-previous 1.0}
               {:value 10N, :coords [{:x 2, :y 3N}], :ratio-vs-previous 2.0}],
              :reached-convergence? false,
              :series [1 0 0 1N 0 1N 1N 0 2N 1N 1N 3N 1N 3N 4N 2N 6N 5N 5N 10N],
              :triangle-seed {:left 1, :right 1},
              :convergence-precision 3,
              :convergence-ratio-with-precision 2.0}))
