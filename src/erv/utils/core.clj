(ns erv.utils.core
  (:require [clojure.spec.alpha :as s]))

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
