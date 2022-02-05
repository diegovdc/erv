(ns erv.cps.similarity
  ;; TODO
  "This file contains code for a WIP implementation of a calculator that should
  would show which CPSs are closer to a given model scale (or some of its degrees).

  Currently only hexanies are being calculated with respecto to 12-edo scales."
  (:require [erv.utils.conversions :as conv]
            [erv.utils.core :as utils]
            [clojure.string :as str]))



(defn twelvulate [scale]
  (map #(-> % (/ 100) float (Math/round) (* 100)) scale))

(comment (twelvulate [0 144]))

(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (Math/pow x 2))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (Math/sqrt (euclidean-squared-distance a b)))

(let [a [0 2 4 6] #_[0 2 3 5 8 13 21]
      b [0 2 4 6 8 10 12]]
  (euclidean-distance a b))
(defn root-to-0 [scale] (map #(- % (first scale)) scale))
(euclidean-distance (range 0 1200 100)
                    '(0.0
                      231.17409353087507
                      315.64128700055267
                      546.8153805314276
                      813.6862861351651
                      933.1290943962624))


(defn +cents [cps]
  (assoc cps :cents (->> cps
                         :scale
                         (map :bounded-ratio)
                         (map conv/ratio->cents)
                         root-to-0
                         (map int))))

(defn rotate-scale [scale-cents n]
  (let [scale* (utils/rotate scale-cents n)]
    (sort (map #(-> % (- (first scale*)) (mod 1200)) scale*))))
(let [scale '(0 182 386 498 701 884)]
  (map (partial rotate-scale scale) (range (count scale))))

(euclidean-distance [32 102 140]
                    [100 300 400 800 1000 1100])

(defn +euclidean-distance* [cps]
  (let [closest-12-edo (twelvulate (:cents cps))]
    (assoc cps
           :closest-12-edo closest-12-edo
           :euclidean-distance (euclidean-distance
                                closest-12-edo
                                (:cents cps)))))
(defn +euclidean-distance [cps]
  (let [scale (:cents cps)]
    (->> scale
         count
         range
         (map (fn [idx]
                (->> idx
                     (rotate-scale scale)
                     (assoc cps :mode idx :cents)
                     +euclidean-distance*)))
         (reduce #(if (and (%1 :euclidean-distance)
                           (< (%1 :euclidean-distance)
                              (%2 :euclidean-distance)))
                    %1 %2)))))

(+euclidean-distance {:cents '(0 204 316 519 702 1018)})
(sort (map #(-> % (- 182) (mod 1200)) '(0 182 386 498 701 884) ))
(defn +gens [factors cps]
  (assoc cps :factors factors))


(comment
  (require '[clojure.math.combinatorics :as combo]
           ;; '[clojure.data.csv :as csv]
           '[clojure.java.io :as io]
           '[erv.utils.core :refer [round2]]
           '[erv.cps.core :as cps])
  (count cps-sorted-by-euclidean-distance)
  (def cps-sorted-by-euclidean-distance
    (-> (range 1 31 2)
        (combo/combinations 7)
        (->> (pmap #(->> (cps/->cps 3 %)
                         cps/set->maps
                         (cps/bound-ratio 2)
                         (cps/maps->data :bounded-ratio)
                         (+gens %)
                         +cents
                         +euclidean-distance))
             (sort-by :euclidean-distance))))
  (->> cps-sorted-by-euclidean-distance
       (map (juxt (comp #(str "Factors: " %) vec :factors)
                  (comp #(str "Cents:" (str/join "," %)) :cents)
                  (comp #(str "Distance: " %) :euclidean-distance)))
       (take 10))
  (def cps-sorted-by-euclidean-distance-up-to-53
    (->> cps-sorted-by-euclidean-distance
         (filter #(<= (apply max (:factors %)) 53))))

  (def cps-sorted-by-euclidean-distance-up-to-23
    (->> cps-sorted-by-euclidean-distance
         (filter #(<= (apply max (:factors %)) 23))))

  (with-open [writer (io/writer "3oo7-similarity-to-12edo-scales-up-to-23.csv")]
    (csv/write-csv writer
                   (->> #_cps-sorted-by-euclidean-distance
                        #_cps-sorted-by-euclidean-distance-up-to-53
                        cps-sorted-by-euclidean-distance-up-to-23
                        (mapv (juxt :factors :mode :cents :closest-12-edo :euclidean-distance ))
                        (mapv (fn [data]
                                (mapv #(cond
                                         (= java.lang.Long (type %)) %
                                         (= java.lang.Double (type %)) (round2 3 %)
                                         :else (str/join " " %))
                                      data)))
                        (into [["Factors" "Mode" "Cents" "Closest 12 edo scale" "Euclidean Distance"]])))))
