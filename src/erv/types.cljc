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

(def Ratio
  #?(:clj [:or :int ratio?]))

(comment
  (m/explain Ratio 3/2)
  (println (keys (m/default-schemas))))

(defn make-scale-meta
  [fields]
  (into [:map
         [:period number?]
         [:size :int]]
        fields))

(def CPSMeta (make-scale-meta
              [[:scale [:enum :cps]]
               [:cps/size :int]
               [:cps/factors [:vector :int]]
               [:cps/normalized-by :int]
               [:cps/type :string]]))

(def Note
  [:map
   [:ratio Ratio]
   [:bounded-ratio Ratio]
   [:bounding-period number?]
   [:degree :int]])

(def ScaleData
  [:map
   [:meta (make-scale-meta [])]
   [:scale [:sequential Note]]])

;; You can validate your data like this:

(comment
  (require '[erv.cps.core :as cps])
  (def scale-data (cps/make 2 [1 3 5 7]))
  (-> scale-data :scale)
  (m/explain ScaleData (-> scale-data
                           (update :scale #(into [] %))))
  (m/explain Note {:set #{7 5},
                   :archi-set #{:c :d},
                   :ratio 35,
                   :bounded-ratio 35/32,
                   :bounding-period 2,
                   :degree 0}))
