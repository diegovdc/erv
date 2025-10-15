(ns erv.types
  (:require [malli.core :as m]
            [malli.util :as mu]))

(def Intish
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  [:or :int [:fn #(instance? clojure.lang.BigInt %)]])

(def MeruBaseData
  [:map
   [:convergence-double double?]
   [:convergence-index int?]
   [:reached-convergence? :boolean]
   [:series [:vector Intish]]
   [:convergence-precision [:or :int :nil]]
   [:convergence-double-with-precision :double]])

(def MeruRecurrentSeriesData
  (mu/merge #'MeruBaseData
            [:map
             [:seed [:vector :int]]]))

(def MeruDiagonalsData
  (mu/merge #'MeruBaseData
            [:map
             [:series-data [:vector
                            [:map
                             [:value #'Intish]
                             [:coords [:vector
                                       [:map
                                        [:x :int]
                                        [:y #'Intish]]]]
                             [:ratio-vs-previous {:optional true} [:maybe :double]]]]]
             [:triangle-seed [:map [:left :int] [:right :int]]]
             [:convergence-precision [:or :int :nil]]
             [:convergence-double-with-precision :double]]))
(comment
  (m/explain MeruDiagonalsData
             (erv.meru.diagonals/diagonals
              {:size 120
               :slope {:x 1 :y 2}
               :pascal-coord->number erv.math.pascals-triangle/default-coord-map}))
  (:errors (m/explain MeruRecurrentSeriesData
                      (erv.meru.recurrent-series/recurrent-series {:seed [1 1 1]
                                                                   :formula :meta-slendro}))))
