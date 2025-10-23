(ns erv.utils.core
  #?(:cljs   (:refer-clojure :exclude [> >= < <= = + - * /  -compare compare numerator denominator integer?
                                       mod rem quot even? odd? pos? zero? inc]))
  (:require
   #?(:cljs [com.gfredericks.exact :as e :refer [* / < = > mod rem zero? inc numerator denominator]])
   [clojure.core :as core]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [erv.utils.exact :as exact.utils]))

(defn validate [spec input]
  (or (s/valid? spec input)
      (throw (ex-info (s/explain-str spec input) {:input input}))))

(defn wrap-at [i coll]
  (let [size (count coll)
        i* (if (core/zero? size) 0 (core/mod i size))]
    (nth coll i* nil)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (core// (Math/round (core/* d factor)) factor)))

(defn rotate [xs n]
  (let [l (count xs)
        off (core/mod (core/+ (core/mod n l) l) l)]
    (concat (drop off xs) (take off xs))))

(defn get-all-rotations [pattern]
  (mapv #(into [] (rotate pattern %))
        (range (count pattern))))

(defn factors [n]
  (filter #(= 0 (rem n %)) (range 2 n)))

(defn prime-factors [n]
  (let [_2 (exact.utils/->exact 2)
        n (exact.utils/->exact n)]
    (loop [n n
           divisor _2
           factors []]
      (if (< n _2)
        factors
        (if (zero? (rem n divisor))
          (recur (/ n divisor) divisor (conj factors divisor))
          (recur n (inc divisor) factors))))))

(comment
  (prime-factors (exact.utils/->exact 1))
  (prime-factors 1))

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
   (let [_1 (exact.utils/->exact 1)
         period* (exact.utils/->exact period)]
     (loop [ratio ratio]
       (cond
         (> period* ratio _1) ratio
         (or (= period* ratio) (= _1 ratio)) _1
         (> ratio period*) (recur (/ ratio period*))
         (< ratio period*) (recur (* ratio period*)))))))

(defn indexes-of [el coll] (keep-indexed #(when (= el %2) %1) coll))

#?(:clj
   (defn ^:export pow [n power]
     (when-not (int? power)
       (throw (ex-info "`power` must be an int" {:power power})))
     (cond
       (zero? power) 1
       (> power 0) (apply * (repeat power n))
       :else (apply / 1 (repeat (abs power) n))))
   :cljs
   (defn ^:export pow [n power]
     (when-not (int? power)
       (throw (ex-info "`power` must be an int" {:power power})))
     (let [n (exact.utils/->exact n)
           power (exact.utils/->exact power)]
       (cond
         (zero? power) (exact.utils/->exact 1)
         (> power (exact.utils/->exact 0)) (apply * (repeat power n))
         :else (apply / (repeat (abs power) n))))))
(comment
  (pow (exact.utils/->exact 2)
       (exact.utils/->exact 0))
  (pow (exact.utils/->exact 2)
       (exact.utils/->exact 3))
  (pow (exact.utils/->exact 2)
       (exact.utils/->exact -3))
  (pow 2 -3))

(defn pattern->degrees
  [pattern]
  (->> pattern
       (reduce (fn [acc el] (conj acc (core/+ el (last acc))))
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
(defn gcd*
  "Greatest common divisor"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn gcd
  "Greatest common divisor"
  [a b]
  (gcd* (exact.utils/->exact a)
        (exact.utils/->exact b)))

;; TODO add tests
(defn lcm*
  "Least common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcm
  "Least common multiple"
  [a b]
  (lcm* (exact.utils/->exact a)
        (exact.utils/->exact b)))

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

(defn decompose-ratio
  ([ratio]
   ;; NOTE the following code does not work in prod, as e/numerator returns nil and doesn't throw on integers
   ;; keeping the code here for documentation purposes and to avoid any refactoring to a similar procedure
   #_(try
       {:numer (numerator ratio) :denom (denominator ratio)}
       (catch #?(:clj Exception :cljs js/Error) _
         {:numer ratio :denom (exact.utils/->exact 1)}))
   #?(:cljs
      (cond
        (e/integer? ratio) {:numer ratio :denom (exact.utils/->exact 1)}
        (e/ratio? ratio) {:numer (numerator ratio) :denom (denominator ratio)})
      :clj
      (cond
        (int? ratio) {:numer ratio :denom 1}
        (ratio? ratio) {:numer (numerator ratio) :denom (denominator ratio)}))))

(defn decompose-ratios
  ([ratios] (mapv decompose-ratio ratios)))

(defn factorize-ratio
  [n]
  (-> n decompose-ratio
      (update :numer prime-factors)
      (update :denom prime-factors)))
(do
  (defn factors->hiccup
    "Outputs hiccup with factors in power notation"
    [factors]
    (if-not (seq factors)
      [:span 1]
      (->> (frequencies factors)
           (sort-by first)
           (map (fn [[factor power]] [:span factor [:sup power]])))))
  (factors->hiccup [3 3 7 5])
  (factors->hiccup []))

(defn make-map-by-key
  "Given a vector of hash-maps with a specific `k`, return a map of `k`->hash-map.
  The user is responsible for providing a unique `k`, otherwise data may be missing."
  [key-fn maps]
  (reduce
   (fn [acc m]
     (assoc acc (key-fn m) m))
   {}
   maps))
