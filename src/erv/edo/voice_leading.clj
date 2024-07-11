(ns erv.edo.voice-leading
  "Tools for finding chords that lead to other chords in an efficient way.
  Inspired by Dmitri Tymoczko's \"A Geometry of Music\" "
  (:require
   [erv.utils.core :refer [get-all-rotations pattern->degrees]]))

(defn degrees->intervals
  [scale-size degrees]
  (let [intervals-minus-rest
        (->> degrees
             sort
             (partition 2 1)
             (mapv (fn [[a b]] (- b a))))
        rest* (- scale-size (apply + intervals-minus-rest))]
    (conj intervals-minus-rest rest*)))

(defn find-closest-chord
  ([edo chord] (find-closest-chord edo chord chord))
  ([edo chord target-chord]
   (let [intervals (degrees->intervals edo target-chord)
         rotations (map pattern->degrees (get-all-rotations intervals))
         transpositions (mapcat
                         #(map (fn [chord] (map (fn [n] (+ n %)) chord)) rotations)
                         (range (* -1 edo) edo))]
     (->> transpositions
          (map (fn [t]
                 (let [mvmnt (map - t chord)]
                   {:initial-chord chord
                    :target-chord t
                    :total-movement (apply + mvmnt)
                    :voice-movement mvmnt
                    :initial-intervals (degrees->intervals edo chord)
                    :target-intervals (degrees->intervals edo t)})))
          (remove #(= (:chord %) chord))
          (sort-by (comp abs :total-movement))))))

(comment
  ;; TODO convert to tests
  (->> (find-closest-chord 14 [1 4 8 12])
       (take 10)
       (map (juxt :target-chord
                  :total-movement
                  :voice-movement
                  (juxt
                   :initial-intervals
                   :target-intervals))))
  (->> (find-closest-chord 12 [0 3 7] [0 3 7])
       (take 10)
       (map (juxt :target-chord :total-movement :voice-movement))))
