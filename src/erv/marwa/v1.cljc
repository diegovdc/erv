(ns erv.marwa.v1
  "Marwa permutations, based on http://anaphoria.com/xen9mar.pdf

  Concepts:
   `G` = generating interval
   `C` = closing interval
   `R` = reciprocal interval: R+C === 2*G
  Rules:
   0. Take a scale an represent it as a chain of generators.
   1. Only sequences which contain n `G` intervals and exactly `C` interval are valid.
   2. A pair of `G`s can generate a new interval plus `C`.
   3. These pairs can be permutated across the generator chain. Both together and each one on their own. All these permutations are valid.
   4. `C` can not be next to `C`.
"
  (:require [erv.utils.core :as utils]))

(defn- get-generator-sequence
  "Given a scale and and a range of intervals, convert the scale into a generator sequence"
  [scale interval]
  (->> scale
       repeat
       (apply concat)
       (partition interval)
       (map (partial apply +))
       ; only take the intervals before the cycle repeats
       (take (count scale))))

(defn- permutate-range-fwd
  "Given a vector and a range (start, end), permutate the values in the range forward by 1 place."
  [start end vec]
  (let [part-1 (subvec vec 0 start)
        part-2 (subvec vec (inc end))
        range* (subvec vec start end)
        permutated-item (subvec vec end (inc end))]
    (into [] (flatten [part-1 permutated-item range* part-2]))))

(defn- all-range-permutations
  "Permutate a range forward across a generator chain."
  [generator-seq range-len initial-index]
  (let [perms-start-indexes
        ;; TODO ask Kraig
        ;; here the `2` prevents the closing interval from being next to itself, but this doesn't seem to work for scales made up of ratios
        (-> (count generator-seq) (- range-len 2 initial-index)
            range
            (->> (map (partial + initial-index))))]
    (reduce (fn [acc idx]
              (let [prev-scale (:generator-seq (last acc))
                    scale  (permutate-range-fwd idx (+ idx range-len) prev-scale)]
                (conj acc {:generator-seq scale
                           :group-size range-len
                           :initial-index (inc idx)})))

            [{:generator-seq generator-seq
              :group-size range-len
              :initial-index initial-index}]
            perms-start-indexes)))

(defn- all-range-permutations-2 [scale range-len initial-index]
  ;; This version removes the `-2` from `perms-start-indexes` so that it works with ratio based scales
  (let [perms-start-indexes
        (-> (count scale) (- range-len initial-index)
            range
            (->> (map (partial + initial-index))))]
    (reduce (fn [acc idx]
              (let [prev-scale (:generator-seq (last acc))
                    scale  (permutate-range-fwd idx (+ idx range-len) prev-scale)]
                (conj acc {:generator-seq scale
                           :group-size range-len
                           :initial-index (inc idx)})))

            [{:generator-seq scale
              :group-size range-len
              :initial-index initial-index}]
            perms-start-indexes)))
#_(all-range-permutations [5 9 5 9 7 7 7 7 7 9] 4 0)
(comment (all-range-permutations-2 (into [] (get-ratio-interval-sequence [9/8 9/8 9/8 256/243 9/8 9/8 256/243] 3)) 2 0))

(defn- all-sub-range-permutations
  "Do the internal permutations for a vector of generator chains"
  [generator-seqs]
  (mapcat (fn [{:keys [generator-seq group-size initial-index]}]
            (map #(assoc % :parent generator-seq)
                 (all-range-permutations generator-seq (dec group-size)
                                         (inc initial-index))))
          generator-seqs))

(defn- all-permutations-for-base-permutation
  "Returns a collection of collections of permutations from a given base-permuation map"
  [{:keys [generator-seq group-size initial-index]}]
  (loop [perms [(all-range-permutations generator-seq group-size initial-index)]]
    (if (-> perms last first :group-size (> 1))
      (recur (conj perms (all-sub-range-permutations (last perms))))
      perms)))

(defn- get-reciprocal-interval
  "R+C === 2*G"
  [G C]
  (- (* 2 G) C))

(defn base-permutations
  "`G` = generating interval
  `C` = closing interval"
  [scale-len G C]
  (let [;; [min* max*] (sort [G C])
        ;; R (- min* (- max* min*))
        R (get-reciprocal-interval G C)
        group [R C]
        max-groups (quot scale-len 2)]

    (map (fn [total]
           {:group-size (* 2 total)
            :generator-seq
            (into [] (flatten
                      [(repeat total group)
                       (repeat (- scale-len 1 (* 2 total)) G)
                       C]))
            :initial-index 0})
         (range 1 max-groups))))

(defn- remove-duplicates [coll]
  (:coll (reduce
          (fn [{:keys [coll already-in] :as acc}
               el]
            (let [gen-seq (el :generator-seq)]
              (if (already-in gen-seq) acc
                  {:coll (conj coll el)
                   :already-in (into already-in
                                     (utils/get-all-rotations gen-seq))})))
          {:coll [] :already-in #{}} coll)))

(defn- interval-seq->degs
  "Assuming 0 as the starting degree, convert an interval sequence into the corresponding degrees."
  [scale-size interval-seq]
  (reduce #(conj %1 (-> %1 last (or 0) (+ %2) (mod scale-size))) [] interval-seq))

(defn- degs->scale
  "Convert a sequence of degrees into a scale representation"
  [scale-size degs]
  (->> (conj degs scale-size) sort (partition 2 1) (map (fn [[a b]] (- b a)))))

(defn intervals->scale
  "Converts a sequence of intervals to an octave-reduced (scale-like) interval representation"
  [scale-size intervals]
  (degs->scale scale-size (interval-seq->degs scale-size intervals)))

(defn- best-sequence?
  "A sequence consisting of n generators (G) and a single closing (C) interval"
  [sequence-freqs]
  (let [two-keys? (= 2 (count (keys sequence-freqs)))
        nG&1C? (contains? (set (vals sequence-freqs)) 1)]
    (and two-keys? nG&1C?)))

(defn- sequence-analysis
  "Analysis data for every generator sequence"
  [sequence]
  (let [freqs (frequencies sequence)]
    {:generator-freqs freqs
     :best-sequence? (best-sequence? freqs)
     :sorted-generators-by-high-freq (->> freqs
                                          (sort-by second >)
                                          (map first))}))

(defn get-possible-generator-sequences
  [scale]
  (map (fn [generator]
         (let [sequence (get-generator-sequence scale generator)]
           (merge {:generator generator ;; in terms of # of notes in scale
                   :sequence  sequence}
                  (sequence-analysis sequence))))
       (range 1 (count scale))))

(defn all-permutations
  [scale-size base-permutations]
  (->> base-permutations
       (map all-permutations-for-base-permutation)
       flatten
       #_(map :generator-seq) ;; generator list
       remove-duplicates
       (map (fn [data]
              (assoc data :scale
                     (intervals->scale scale-size
                                       (data :generator-seq))))) ;; scale list
       #_(remove #(some zero? %))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO (WIP) marwa permutations for ratios
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-ratio-interval-sequence-2
  [scale interval]
  (->> scale
       repeat
       (apply concat)
       (partition interval)
       (map (partial apply *))
       (take (count scale))))

(comment (get-ratio-interval-sequence-2 [9/8 9/8 10/9 16/15 9/8 9/8 256/243] 3)
         (get-ratio-interval-sequence-2 [32/27 9/8 135/128 4096/3645 9/8 135/128 16/15] 3))

;;  These next two functions will use all-range-permutations-2 see comments on that function

(defn- all-sub-range-permutations-2
  [scales]
  (mapcat (fn [{:keys [generator-seq group-size initial-index]}]
            (map #(assoc % :parent generator-seq)
                 (all-range-permutations-2 generator-seq (dec group-size)
                                           (inc initial-index))))
          scales))

(defn- all-permutations-for-base-permutation-2
  [{:keys [generator-seq group-size initial-index]}]
  (loop [perms [(all-range-permutations-2 generator-seq group-size initial-index)]]
    (if (-> perms last first :group-size (> 1))
      (recur (conj perms (all-sub-range-permutations-2 (last perms))))
      perms)))

(comment
;;; testing the permutation algorithm with some of the cases from the xen9mar.pdf
  ;; Fig. 2
  (->> [{:group-size 2
         :generator-seq (into []
                              (get-ratio-interval-sequence-2
                               [9/8 9/8 9/8 256/243 9/8 9/8 256/243] 3))
         :initial-index 0}]
       (map all-permutations-for-base-permutation-2)
       flatten
       (map :generator-seq)
       ;; remove-duplicates
       #_(map (fn [intervals] (intervals->scale intervals)))))
