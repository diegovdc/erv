(ns erv.utils.core
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(defn validate [spec input]
  (or (s/valid? spec input)
      (throw (ex-info (s/explain-str spec input) {:input input}))))

(defn wrap-at [i coll]
  (let [size (count coll)
        i* (if (zero? size) 0 (mod i size))]
    (nth coll i* nil)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn rotate [xs n]
  (let [l (count xs)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off xs) (take off xs))))

(defn get-all-rotations [pattern]
  (mapv #(into [] (rotate pattern %))
        (range (count pattern))))

(defn factors [n]
  (filter #(= 0 (rem n %)) (range 2 n)))

(defn prime-factors [n]
  (loop [n n divisor 2 factors []]
    (if (< n 2)
      factors
      (if (zero? (rem n divisor))
        (recur (/ n divisor) divisor (conj factors divisor))
        (recur n (inc divisor) factors)))))
(comment
  (prime-factors 2))

(defn coprime? [& ns]
  (->> ns (map (comp set prime-factors)) (apply set/intersection) empty?))

(defn coprimes
  ([n] (coprimes n n))
  ([n limit]
   (->> (range 1 limit)
        (filter #(and (coprime? % n)
                      (not= % n))))))

(defn interval [ratio-a ratio-b] (/ ratio-b ratio-a))

(defn period-reduce
  ([ratio] (period-reduce 2 ratio))
  ([period ratio]
   (loop [ratio ratio]
     (cond
       (> period ratio 1) ratio
       (or (= period ratio) (= 1 ratio)) 1
       (> ratio period) (recur (/ ratio period))
       (< ratio period) (recur (* ratio period))))))

(defn indexes-of [el coll] (keep-indexed #(when (= el %2) %1) coll))

(defn ^:export pow [n power]
  (when-not (int? power)
    (throw (ex-info "`power` must be an int" {:power power})))
  (cond
    (zero? power) 1
    (> power 0) (apply * (repeat power n))
    :else (apply / 1 (repeat (abs power) n))))

(defn pattern->degrees
  [pattern]
  (->> pattern
       (reduce (fn [acc el] (conj acc (+ el (last acc))))
               [0])
       drop-last))

(defn pick-pattern
  "Create a subscale using a pattern (MOS or other)"
  [scale pattern]
  (let [indexes (pattern->degrees pattern)]
    (reduce
     (fn [subscale i]
       (if-let [note (nth scale i nil)]
         (conj subscale note)
         (reduced subscale)))
     []
     indexes)))

(defn degree-stack
  "Generate a stack of degrees from a single generator"
  [{:keys [scale gen offset]}]
  (sort-by :gen/index
           (loop
            [subset #{}
             ratio-subset #{}
             offset offset
             gen-index 0]
             (let [i (mod offset (count scale))
                   new-note (assoc (nth scale i)
                                   :gen/index gen-index)
                   ratio (:bounded-ratio new-note)]
               (if (ratio-subset ratio)
                 subset
                 (recur (conj subset new-note)
                        (conj ratio-subset ratio)
                        (+ offset gen)
                        (inc gen-index)))))))

(defn scale-intervals
  "Get the intervals of a scale"
  [scale]
  (let [sorted-scale (->> scale
                          (sort-by :bounded-ratio))
        last-interval [(last sorted-scale)
                       (update (first sorted-scale)
                               :bounded-ratio #(* % (:bounding-period (first sorted-scale))))]]
    (->> sorted-scale
         (partition 2 1)
         (into [])
         (#(conj % last-interval))
         (map (fn [[a b]] (interval (:bounded-ratio a)
                                    (:bounded-ratio b)))))))
