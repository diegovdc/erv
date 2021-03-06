(ns erv.utils.core
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defn validate [spec input]
  (or (s/valid? spec input)
      (throw (ex-info (s/explain-str spec input) {:input input}))))

(defn wrap-at [i coll]
  (let [i* (mod i (count coll))]
    (nth coll i*)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off a) (take off a))))

(defn prime-factors [n]
  (loop [n n divisor 2 factors []]
    (if (< n 2)
      factors
      (if (zero? (rem n divisor))
        (recur (/ n divisor) divisor (conj factors divisor))
        (recur n (inc divisor) factors)))))

(defn coprime? [& ns]
  (->> ns (map (comp set prime-factors)) (apply set/intersection) empty?))
