(ns erv.meru.core
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.meru.diagonals]
   [erv.meru.recurrent-series]
   [erv.mos.v3.core :refer [gen->mos-ratios]]
   [erv.utils.core :refer [round2]]))

(def recurrent-series #'erv.meru.recurrent-series/recurrent-series)

(def diagonals #'erv.meru.diagonals/diagonals)

(defn convergence-mos-data
  ([convergence-double] (convergence-mos-data {} convergence-double))
  ([{:keys [period max-size]
     :or {period 2 max-size 100}}
    convergence-double]
   (->> (gen->mos-ratios (rationalize (round2 3 convergence-double)) period max-size)
        (map :meta))))

(defn convergence-mos-data-summary
  ([meru-diagonals-or-series-data] (convergence-mos-data-summary {} meru-diagonals-or-series-data))
  ([{:keys [_period _max-size] :as calc-config}
    {:keys [convergence-double] :as _meru-diagonals-or-series-data}]
   (->> convergence-double
        (convergence-mos-data calc-config)
        (map (fn [meta]
               (select-keys meta [:size
                                  :mos/pattern.name
                                  :mos/sL-ratio.float
                                  :mos/s.cents
                                  :mos/L.cents]))))))

(comment
  (do
    #_(def test1
        (let [seed [1 1 1]
              period 2]
          (->> (recurrent-series {:seed (mapv bigint seed)
                                  :i1 2
                                  :i2 3
                                  :f (fn [a b] (+ a b))})
               (partition 9 1)
               (map (fn [seq*]
                      (let [seq** (sort (set (map (partial within-bounding-period period)
                                                  seq*)))
                            indexed-seq (->> seq**
                                             (map-indexed (fn [i x] {x i}))
                                             (apply merge))
                            min* (/ (apply max seq**) 2)]
                        (->> seq**
                             (#(combo/combinations % 3))
                             (reduce (fn [acc ns]
                                       (let [diffs (->> ns
                                                        sort
                                                        (partition 2 1)
                                                        (map (fn [[a b]] (- b a))))]
                                         (if (= 1 (count (set diffs)))
                                           (update acc :proportional-triads
                                                   conj {:ratios ns
                                                         :degrees (->> (map indexed-seq ns))
                                                         :diff (first diffs)})
                                           acc)))
                                     {:meta {:scale :meru
                                             :period period
                                             :seed seed
                                             :size (count seq**)}
                                      :scale (map (fn [r]
                                                    {:ratio r
                                                     :bounded-ratio (/ r min*)
                                                     :bounding-period 2})
                                                  seq**)})
                             #_(#(assoc-in % [:meta :total-triads] (count (:proportional-triads %))))
                             #_(#(assoc-in % [:meta :proportional-triads] (:proportional-triads %)))
                             #_(#(dissoc % :proportional-triads))))))
               #_(remove (comp empty? :proportional-triads :meta)))))
    (def test1
      (let [seed [1 1]
            period 2]
        (->> (recurrent-series {:seed (mapv bigint [1 1])
                                :i1 1
                                :i2 2}
                               ;; :f (fn [a b] (+ a b))
                               )
             :series
             (partition 21 1)
             (map (fn [seq*]
                    (let [seq** (sort (set (map (partial within-bounding-period period)
                                                seq*)))
                          indexed-seq (->> seq**
                                           (map-indexed (fn [i x] {x i}))
                                           (apply merge))
                          min* (/ (apply max seq**) 2)]
                      (->> seq**
                           (#(combo/combinations % 3))
                           (reduce (fn [acc ns]
                                     (let [diffs (->> ns
                                                      sort
                                                      (partition 2 1)
                                                      (map (fn [[a b]] (- b a))))]
                                       (if (= 1 (count (set diffs)))
                                         (update acc :proportional-triads
                                                 conj {:ratios ns
                                                       :degrees (->> (map indexed-seq ns))
                                                       :diff (first diffs)})
                                         acc)))
                                   {:meta {:scale :meru
                                           :period period
                                           :seed seed
                                           :size (count seq**)}
                                    :scale (map (fn [r]
                                                  {:ratio r
                                                   :bounded-ratio (/ r min*)
                                                   :bounding-period 2})
                                                seq**)})
                           #_(#(assoc-in % [:meta :total-triads] (count (:proportional-triads %))))
                           #_(#(assoc-in % [:meta :proportional-triads] (:proportional-triads %)))
                           #_(#(dissoc % :proportional-triads))))))
             #_(remove (comp empty? :proportional-triads :meta)))))

    (->> test1
         #_#_#_(sort-by (comp :size :meta) >)
             first
           :scale

         #_(map (comp (juxt :size :total-triads) :meta)))))

(defn harmonic-mean [a b]
  (/ (* 2 a b)
     (+ a b)))

(harmonic-mean 3 6)
(comment
  ;; integer harmonic means between 1-1000

  (->> (combo/combinations (range 1 1001) 2)
       (map (fn [[a c]]
              {:a a :b (harmonic-mean a c) :c c}))
       (filter (fn [{:keys [a b c]}]
                 (and (= (int b) b)
                      (>= 2 (/ c a)))))))
