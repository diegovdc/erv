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

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off a) (take off a))))

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

(defn pow [n power]
  (apply * (repeat power n)))
