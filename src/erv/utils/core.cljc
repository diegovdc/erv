(ns erv.utils.core
  (:require
   [clojure.set :as set]
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

(defn pick-degrees
  [scale degrees]
  (map #(wrap-at % scale) degrees))

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

;; TODO add tests
(defn gcd
  "Greatest common divisor"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

;; TODO add tests
(defn lcm
  "Least common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

;; TODO add tests
(defn lcm-of-list
  "Find the least common multiple of a list of numbers"
  [nums]
  (reduce lcm nums))

;; TODO add tests
(defn gcd-of-list
  "Find the greatest common divisor of a list of numbers"
  [nums]
  (reduce gcd nums))
