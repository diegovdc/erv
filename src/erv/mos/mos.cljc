(ns erv.mos.mos
  "From http://anaphoria.com/wilsonintroMOS.html

  A Moment of Symmetry is a scale that consists of:
  1. A generator (of any size, for example a 3/2 or a fifth in 12 equal
     temperament) which is repeatedly superimposed but reduced within
  2. An Interval of Equivalence commonly called a period (of any size, for
     example most commonly an octave).
  3. A Moment of Symmetry is formed where each scale degree or scale unit size
     will be represented by no more than two sizes and two sizes only
     (Large = L and small = s).
  4.The relative number of L and s intervals is coprime, i.e. they share no
    common factors other than 1.
  5. The numerator (generator) and denominator (period) representing MOS are
     also co-prime."
  (:require [taoensso.timbre :as timbre]
            [erv.utils.core :refer [coprime?]]))

(do
  (defn get-diffs [points]
    (->> (partition 2 1 points)
         (mapv (fn [[a b]] (- b a)))))
  (get-diffs (range 12)))

(defn get-mos-points [period generator]
  (loop [mos-points [0]]
    (let [next-point (mod (+ (last mos-points) generator) period)]
      (if-not (= 0 next-point)
        (recur (conj mos-points next-point))
        (conj mos-points period)))))

(defn mos-points->mos
  [mos-points]
  (->> (drop-last 1 mos-points)
       (reduce
        (fn [{:keys [moses waiting]} point]
          (let [points (into [] (sort (concat waiting (last moses) [point])))
                intervals (get-diffs points)]
            (->> intervals frequencies vals (apply coprime?))
            (cond
              (= #{1 2} (set intervals)) ;; NOTE Do we really not want any more MOS?
              (reduced {:moses (conj moses points) :waiting []})
              (and (<= (count (set intervals)) 2)
                   (->> intervals frequencies vals (apply coprime?)))
              {:moses (conj moses points) :waiting []}
              :else
              {:moses moses :waiting (conj waiting point)})))
        {:moses [] :waiting [(last mos-points)]})
       :moses
       (#(conj % (range (count mos-points))))))

(comment (make-mos 12 5))

(defn mos-as-intervals [mos] (mapv get-diffs mos))


(defn make-mos [period generator]
  (let [true-mos? (coprime? period generator)]
    (when-not true-mos?
      (timbre/warn "The generated data is not a true MOS because the period (" period ") and generator (" generator ") are not coprime."))
    (with-meta
      (->> (get-mos-points period generator)
           mos-points->mos
           mos-as-intervals)
      {:true-mos? true-mos?})))
