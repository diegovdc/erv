(ns erv.cps.utils)

;; implement correctly
#_(defn get-cps-diads [cps-scale]
    (->> (combo/combinations cps-scale 2)
         (filter (fn [[a b]]
                   (seq (set/intersection (:archi-set a) (:archi-set b)))))
         (map (fn [[a b]] [(:bounded-ratio a) (:bounded-ratio b)]))))

(comment
  (require '[erv.cps.core :as cps]))

(def ^:private make-bounded-ratio->deg-map
  (memoize
   (fn  [cps]
     (->> cps :scale
          (map-indexed #(assoc %2 :degree %1))
          (reduce #(assoc %1 (:bounded-ratio %2) (:degree %2)) {})))))

(def ^:private subcps-degrees*
  (memoize
   (fn  [bounded-ratio->deg-map subcps]
     (map (comp bounded-ratio->deg-map :bounded-ratio)
          (:scale subcps)))))

(defn subcps-degrees
  "The subcps degrees relative to another cps.
  NOTE: if the subcps is not really a subset of the cps, then some degrees may be `nil`"
  [cps subcps]
  (let [ratio->deg-map (make-bounded-ratio->deg-map cps)]
    (subcps-degrees* ratio->deg-map subcps)))
