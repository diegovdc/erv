(ns erv.constant-structures.core
  "A constant structure is a scale where every interval is subtended by the same number of steps."
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.utils.core :refer [interval round2]]))

(defn maybe-round
  [n]
  #?(:clj (if (rational? n)
            n
            (round2 6 n))
     :cljs (round2 6 n)))

(defn get-intervals
  [note-pair]
  (->> note-pair
       ((fn [[a b]]
          {(maybe-round (interval (:bounded-ratio a)
                                  (:bounded-ratio b)))
           {:steps #{(- (:index b)
                        (:index a))}
            :intervals [[a b]]}}))))

(defn analyze
  [scale]
  (let [interval-data (->> (combo/combinations
                            (map-indexed #(assoc %2 :index %1) scale)
                            2)
                           (map get-intervals)
                           (apply merge-with (partial merge-with concat))
                           (map (fn [[interval data]]
                                  {interval (update data :steps (partial into #{}))}))
                           (apply merge)
                           (map (fn [[interval* data]]
                                  [interval*
                                   (update data
                                           :intervals
                                           (partial
                                            map
                                            (fn [pair]
                                              {:steps (->> pair
                                                           (map :index)
                                                           reverse
                                                           (apply -))
                                               :interval
                                               (map
                                                :bounded-ratio
                                                pair)})))]))
                           (sort-by first))
        non-cs-intervals (->> interval-data
                              (filter (fn [[_interval {:keys [steps]}]]
                                        (> (count steps) 1))))]
    {:interval-data interval-data
     :non-cs-intervals {:total (count non-cs-intervals)
                        :intervals non-cs-intervals}
     :constant-structure? (empty? non-cs-intervals)}))
#_(:constant-structure? (analyze
                         (:scale (edo/from-pattern [2 2 3 2 3]))))
#_(:constant-structure? (analyze
                         (:scale (edo/from-pattern [2 4 3 1 3]))))
#_(:constant-structure? (analyze
                         (:scale (cps/make 2 [11 13 5 7]))))

(:constant-structure? (analyze
                       (:scale (cps/make 2 [1 3 5 7 9]))))
(:constant-structure? (analyze
                       (:scale (edo/from-pattern [2 2 1 2 2 2 1]))))
#_(:non-cs-intervals (analyze
                      (:scale (cps/make 3 (map #(* 1 %) [1 3 7 9 11 15])
                                        :norm-fac (* 1 9 11)))))
(:non-cs-intervals (analyze
                     ;; taken from http://anaphoria.com/Wilson1-3-7-9-11-15x3_pascal_eikosany.scl
                     ;; See also tieminos.learning.cps-lumatone.analysis.1-3-7-9-11-15
                    (:scale
                     (-> (cps/make 3 (map #(* 1 %) [1 3 7 9 11 15])
                                   :norm-fac (* 1 9 11))
                         (update :scale
                                 (comp
                                  #(map-indexed (fn [i n] (assoc n :index i)) %)
                                  #(sort-by :bounded-ratio %)
                                  concat)
                                 [#_{:set #{4/3 7 15}
                                     :ratio 140/99
                                     :bounded-ratio 140/99
                                     :bounding-period 2}
                                  #_{:set #{11 9}
                                     :set-vecs [[11 9 9] [11 9 3 3]]
                                     :ratio 9
                                     :bounded-ratio 18/16
                                     :bounding-period 2}])))))

#_(< 12/11 19/17 7/6)
