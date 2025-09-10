(ns erv.meru.core
  (:require [clojure.math.combinatorics :as combo]
            [erv.cps.core :refer [within-bounding-period]]
            [erv.constant-structures.graphics :as sketch]
            [erv.meru.recurrent-series]
            [erv.meru.diagonals]))

(def recurrent-series #'erv.meru.recurrent-series/recurrent-series)

(def diagonals #'erv.meru.diagonals/diagonals)

(comment
  (do
    (def test1
      (let [seed [1 1 1]
            period 2]
        (->> (recurrent-series (mapv bigint seed)
                               :i1 3
                               :i2 2
                               :f (fn [a b] (+ a b)))
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
                           (#(assoc-in % [:meta :total-triads] (count (:proportional-triads %))))
                           (#(assoc-in % [:meta :proportional-triads] (:proportional-triads %)))
                           (#(dissoc % :proportional-triads))))))
             (remove (comp empty? :proportional-triads :meta)))))
    (def test1
      (let [seed [1 1]
            period 2]
        (->> (recurrent-series (mapv bigint seed)
                               :i1 1
                               :i2 2
                            ;; :f (fn [a b] (+ a b))
                               )
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
                           (#(assoc-in % [:meta :total-triads] (count (:proportional-triads %))))
                           (#(assoc-in % [:meta :proportional-triads] (:proportional-triads %)))
                           (#(dissoc % :proportional-triads))))))
             (remove (comp empty? :proportional-triads :meta)))))

    (->> test1
         (sort-by (comp :size :meta) >)
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
