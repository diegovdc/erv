(ns erv.meru.core
  (:require [clojure.math.combinatorics :as combo]
            [erv.cps.core :refer [within-bounding-period]]
            [erv.constant-structures.graphics :as sketch]))

(defn seq-ratios [recurrent-seq]
  (->> recurrent-seq
       (partition 2 1)
       (map (fn [[a b]] (double (/ b a))))))

(declare converges-at)
(defn converges-at [recurrent-seq & {:keys [ignore-first]
                                     :or {ignore-first 0}}]
  (->> recurrent-seq
       (drop ignore-first)
       seq-ratios
       (partition 5 1)
       (take-while (fn [ns] (apply not= ns)))
       count
       (+ ignore-first)))

(do
  (defn recurrent-seq [seed & {:keys [i1 i2 f]
                               :or {f (fn [a b] (+ a b))}}]
    (let [seed* (vec seed)]
      (loop [seq* seed*
             a (first (take-last i1 seed))
             b (first (take-last i2 seed))]
        (let [seq** (conj seq* (f a b))
              a* (first (take-last i1 seq**))
              b* (first (take-last i2 seq**))]
          (if (apply = (seq-ratios (take-last 6 seq**)))
            seq**
            (recur seq** a* b*))))))
  (let [r (recurrent-seq (map bigint [1 1 1 2 3 4 5 6])
                         :i1 6
                         :i2 7
                         :f (fn [a b] (+ a b))
                         :seq-len 40)]
    {:convergence (last (seq-ratios r))
     :converges-at (converges-at r)
     :seq r})

  )

(defn within-period [period seq*]
  (let [max* (apply max seq*)
        min* (/ max* period)]
    (map (fn [n]
           (if (> min* n)
             (* n (int (Math/ceil (/ min* n))))
             n))
         seq*)))

(comment
  (do
    (def test1
      (let [seed [1 1 1]
            period 2]
        (->> (recurrent-seq (mapv bigint seed)
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
        (->> (recurrent-seq (mapv bigint seed)
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
