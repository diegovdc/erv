(ns erv.mos.v2.submos
  (:require
   [erv.mos.mos :as mos]
   [erv.mos.submos :as submos :refer  [make-submos-for-pattern]]))

(defn take-until [target mos]
  (reduce (fn [acc x] (if (= target (apply + acc))
                        (reduced acc)
                        (conj acc x)))
          []
          mos))

(defn get-submos-pattern [mos sub]
  (loop [sub sub
         mos mos
         submos []]
    (if-not (seq mos)
      submos
      (let [submos-val (count (take-until (first sub) mos))]
        (recur (rest sub)
               (drop submos-val mos)
               (conj submos submos-val))))))

(defn get-all-subs [generator mos-row]
  (let [mos (mos/make-mos (apply + mos-row) generator)]
    (rest (take-while #(not= % mos-row) mos))))

(defn make-all-submos [generator mos-row]
  (->> mos-row
       (get-all-subs generator)
       (map (fn [sub-row]
              (let [pattern (get-submos-pattern mos-row sub-row)]
                {:pattern pattern
                 :period (apply + pattern)
                 ;; :generator (get-generator-from-pattern pattern)
                 :true-submos? true
                 :submos
                 (make-submos-for-pattern
                  mos-row
                  pattern)})))))

(comment
  (make-all-submos 5 [1 2 2 1 2 2 2])

  (get-all-subs 5 [4 1 4 1 4 1 4 1 4 1 4 1 1])

  (make-submos-for-pattern
   [4 1 4 1 4 1 4 1 4 1 4 1 1]
   (get-submos-pattern [4 1 4 1 4 1 4 1 4 1 4 1 1]
                       [5 5 5 5 5 5 1]))
  (make-submos-for-pattern
   [1 2 2 1 2 2 2]
   (get-submos-pattern
    [1 2 2 1 2 2 2]
    [3 2 3 2 2])))
