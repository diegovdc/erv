(ns erv.mos.v2.submos
  (:require
   [erv.mos.mos :as mos]
   [erv.mos.submos :refer [deduplicate make-submos-for-pattern]]
   [clojure.string :as str]))

;; from 5)31

(defn take-until [target mos]
  (reduce (fn [acc x] (if (= target (apply + acc))
                        (reduced acc)
                        (conj acc x)))
          []
          mos))
#_(take-until 5 [4 1 4 1 4 1 4 1 4 1 4 1 1])
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

(get-submos-pattern [4 1 4 1 4 1 4 1 4 1 4 1 1]
                    [5 5 5 5 5 5 1])

(mos/make-mos 31 5)

(defn get-all-subs [generator mos-row]
  (let [mos (mos/make-mos (apply + mos-row) generator)]
    (rest (take-while #(not= % mos-row) mos))))

(comment

  (get-all-subs 5 [4 1 4 1 4 1 4 1 4 1 4 1 1])

  (make-submos-for-pattern
   [4 1 4 1 4 1 4 1 4 1 4 1 1]
   (get-submos-pattern [4 1 4 1 4 1 4 1 4 1 4 1 1]
                       [5 5 5 5 5 5 1]))

  (map #(make-submos-for-pattern
         [4 1 4 1 4 1 4 1 4 1 4 1 1]
         (get-submos-pattern [4 1 4 1 4 1 4 1 4 1 4 1 1] %))
       (get-all-subs 5 [4 1 4 1 4 1 4 1 4 1 4 1 1])))

(spit "11th-row-of-50-out-of-311.txt"
      (->> mos
           (take-while #(not= % last-row))
           rest
           (mapcat #(make-submos-for-pattern
                     last-row
                     (get-submos-pattern last-row %)))
           (let [generator 50
                 mos (mos/make-mos generator 311)
                 last-row (nth mos 10)])
           (map :mos)
           (deduplicate #{})
           :mos-set
           (sort-by count)
           (map #(str/join " "))
           #(str/join "\n")))
