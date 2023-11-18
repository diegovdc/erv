(ns erv.cps.utils
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [wrap-at]]))

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

(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))

(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(defn +degrees [cps]
  (update cps :scale +degree))

(defn make-degree->note [cps]
  (->> cps :scale +degree
       (reduce (fn [m {:keys [degree] :as note}]
                 (assoc m degree note))
               {})))

(comment
  (make-degree->note (erv.cps.core/make 2 [1 3 5 7]))
  )

(do
  (defn make-set->degrees-map
    [{:keys [scale] :as _cps}]
    (->> scale
         (map-indexed (fn [i {:keys [set]}] [set i]))
         (into {})))

  (defn make-degree->sets-map
    [{:keys [scale] :as _cps}]
    (->> scale
         (map-indexed (fn [i {:keys [set]}] [i set]))
         (reduce (fn [m [deg set]] (update m deg (fnil conj #{}) set))
                 {})))
  (make-degree->sets-map (erv.cps.core/make 2 [1 3 5 7])))


(defn- set-d1-intersection?
  [set1 set2]
  (= 1 (count (set/difference set1 set2))))

(do
  (defn harmonic-sets
    "Returns a set of triads (as cp note sets) for a specific degree of a cps
  A harmonic set is a set where all notes are connected with any other by all of its factors except one."
    [{:keys [scale graphs] :as _cps} set-size degree]
    (let [degree* (wrap-at degree scale)
          degree-set (:set degree*)
          deg-edges-sets ((:simple graphs) degree-set)]

      (->> (combo/combinations deg-edges-sets (dec set-size))
           (map #(set (conj % degree-set)))
           (filter #(->> (combo/combinations % 2)
                         (map (partial apply set-d1-intersection?))
                         (every? true?)) )
           set)))

  (harmonic-sets (erv.cps.core/make 3 [1 3 5 7 9 11])
                 4
                 0))


(defn harmonic-set-degrees
  "Returns a list of harmonic sets (as degrees) for a specific degree of a cps
  A harmonic set is a set where all notes are connected with any other by all of its factors except one."
  [cps set-size degree]
  (let [sets (harmonic-sets cps set-size degree)
        set->degrees (make-set->degrees-map cps)]
    (sort (map (comp #(into [] %)
                     sort
                     (partial map set->degrees))
               sets))))=

(comment
  (=
    [#{#{#{7 5} #{3 5} #{7 3}}
       #{#{7 5} #{3 5} #{1 5}}
       #{#{7 1} #{7 5} #{7 3}}
       #{#{7 1} #{7 5} #{1 5}}}
    '((0 1 4) (0 1 5) (0 2 4) (0 2 5))]
    (let [cps (erv.cps.core/make 2 [1 3 5 7])
          set-size 3]
      [(harmonic-sets cps set-size 0)
       (harmonic-set-degrees cps set-size 0)])))
