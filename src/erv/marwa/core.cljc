(ns erv.marwa.core
  (:require [clojure.string :as str]
            [erv.mos.mos :as mos]
            [erv.utils.core :refer [coprimes]]))

;; cf. pg 3 of  http://anaphoria.com/xen9mar.pdf

;;                  S M N G D R P S
(def kalyan-fourths [5 4 4 4 4 4 4])
(def kalayan [2 2 2 1 2 2 1])
;;S R G M P D N S    índices
,,[2 2 2 1 2 2 1]
;;S R G m P D N S -- 2->3
,,[2 2 1 2 2 2 1]
;;S R G M P D n S -- 6->7
,,[2 2 2 1 2 1 2]
;;S R G m P D n S -- 2->3 y 5->6
,,[2 2 1 2 2 1 2]
;;S R g M P D N S -- 1->2
,,[2 1 3 1 2 2 1]
;;S R g m P D n S -- 1->2, 2->3
,,[2 1 2 2 2 2 1]
;;S R g m P D n S -- 1->2, 2->3 y 5->6
,,[2 1 2 2 2 1 2]


(do
  (defn get-interval-sequence [scale interval]
    (->> scale
         repeat
         (apply concat)
         (partition interval)
         (map (partial apply +))
         (take (count scale))))         ; only take the intervals before the cycle repeats
  (get-interval-sequence [1 1 1 4 1 1 4] 2))



(do
  (defn- make-interval-seq [scale generator]
    (->> (mos/get-mos-points (count scale) generator)
         (drop-last 1)
         (drop 1)))
  (make-interval-seq kalayan 5))

(do
  (defn marwa-seq [scale generator]
    (loop [interval-seq (make-interval-seq scale generator)
           m-perms []]
      (let [current-el (first interval-seq)
            next-seq (rest interval-seq)]
        (cond (not (seq next-seq)) m-perms ;; don't recur for the last degree of the interval-seq
              (not (seq m-perms)) (recur next-seq (conj m-perms [current-el]))
              :else (recur next-seq
                           (concat m-perms
                                   (loop [m-perms* m-perms
                                          sub-perms [[current-el]]]
                                     (let [current-perm-el (first m-perms*)
                                           next-perms (rest m-perms*)]
                                       (cond
                                         (not current-perm-el) sub-perms
                                         :else
                                         (recur next-perms
                                                (conj sub-perms
                                                      (conj current-perm-el current-el))))))))))))
  (marwa-seq kalayan 3))
(defn mod* [scale index]
  (mod index (count scale)))
(do
  (defn marwa [scale generator]
    (->> (marwa-seq scale generator)
         (map
          (fn [perms]
            (reduce
             #(-> %1
                  (update-in [(mod* scale (dec %2))] dec)
                  (update-in [(mod* scale %2)] inc))
             scale
             perms)))
         (map (fn [scale] {:generator-seq scale
                          :fourths (get-interval-sequence scale generator)}))
         (#(conj % {:generator-seq scale :fourths (get-interval-sequence scale generator)}))))
  (marwa [1 2 2 1 2 2 2] 4))
#_(clojure.pprint/pprint (marwa-seq))
#_(clojure.pprint/pprint (marwa kalayan 3))
#_(do

    (defn get-marwa-seq [points]
      {:points (reduce #(conj %1 (+ (last %1) %2)) [0] points)
       :interval-indexes nil }
      )
    (get-marwa-seq kalyan))

(comment
  (->> (marwa-seq kalayan 3)
       (map-indexed (fn [i transforms]
                      (str (inc i) "."
                           (str/join ", and "
                                     (map #(str "lower degree " (inc %) " by one semitone")
                                          (sort transforms))))))
       (str/join "\n")
       (println)))

(apply + '(4 3 4 3 4 3 3))
(apply + '(5 4 3 3 3 3 3))
(apply + '(9 9 8 9 8 9 8))
(apply + '(8 10 8 9 8 9 8))

(apply + '(6 5 5 5 5 5 5))




;; ======================V2====================
;; DO this
[[6 5 5 5 5 5 5]

 [6 4 6 5 5 5 5]
 [6 4 5 6 5 5 5]
 [6 4 5 5 6 5 5]
 [6 4 5 5 5 6 5]
 [6 5 4 6 5 5 5]
 [6 5 4 5 6 5 5]
 [6 5 4 5 5 6 5]
 [6 5 5 4 6 5 5]
 [6 5 5 4 5 6 5]
 [6 5 5 5 4 6 5]

 [6 4 6 4 6 5 5]
 [6 4 6 4 5 6 5]

 [6 4 6 5 4 6 5]
 [6 4 5 6 4 6 5]
 [6 5 4 6 4 6 5]]

;; G = generating interval
;; C = closing interval
;; Rules
;; C can not be next to C

(do
  ;; NOT USED
  (defn permutate-fwd
    "Takes a vector and an `index` and permutates the `index` with the `index+1`"
    [vec index]
    {:pre [(> (count vec) (inc index))]}
    (into (conj (subvec vec 0 index)
                ;; the permutation
                (nth vec (inc index))
                (nth vec index))
          (subvec vec (+ 2 index))))
  (permutate-fwd [6 4 6 5 5 4 5] 5))

(do
  (defn permutate-range-fwd [vec start end]
    (let [part-1 (subvec vec 0 start)
          part-2 (subvec vec (inc end))
          range* (subvec vec start end)
          permutated-item (subvec vec end (inc end))]
      (into [] (flatten [part-1 permutated-item range* part-2]))))
  (permutate-range-fwd [5 9 7 7 7 7 7 7 7 9] 0 2))

(do

  (defn all-range-permutations [scale range-len initial-index]
    (let [perms-start-indexes
          ;; TODO ask Kraig
          ;; here the `2` prevents the closing interval from being next to itself, but this doesn't seem to work for scales made up of ratios
          (-> (count scale) (- range-len 2 initial-index)
              range
              (->> (map (partial + initial-index))))]
      (reduce (fn [acc idx]
                (let [prev-scale (:generator-seq (last acc))
                      scale  (permutate-range-fwd prev-scale idx (+ idx range-len))]
                  (conj acc {:generator-seq scale
                             :group-size range-len
                             :initial-index (inc idx)
                             }))
                )
              [{:generator-seq scale
                :group-size range-len
                :initial-index initial-index}]
              perms-start-indexes)))

  (defn all-range-permutations-2 [scale range-len initial-index]
    ;; This version removes the `-2` from `perms-start-indexes` so that it works with ratio based scales
    (let [perms-start-indexes
          (-> (count scale) (- range-len initial-index)
              range
              (->> (map (partial + initial-index))))]
      (reduce (fn [acc idx]
                (let [prev-scale (:generator-seq (last acc))
                      scale  (permutate-range-fwd prev-scale idx (+ idx range-len))]
                  (conj acc {:generator-seq scale
                             :group-size range-len
                             :initial-index (inc idx)
                             }))
                )
              [{:generator-seq scale
                :group-size range-len
                :initial-index initial-index}]
              perms-start-indexes)))
  #_(all-range-permutations [5 9 5 9 7 7 7 7 7 9] 4 0)
  (comment (all-range-permutations-2 (into [] (get-ratio-interval-sequence [9/8 9/8 9/8 256/243 9/8 9/8 256/243] 3)) 2 0)))


;;; I think this is DONE
;;; TODO WIP `all-range-permutations` now generates all the main blocks,
;;; but for example, the generated permutations of group-size 3
;;; should then used as a base for permutations of size 2 and size 1,
;;; the ones of size 4, should have size 3, 2 and 1, etc...


(do
  (defn all-sub-range-permutations [generator-seqs]
    (mapcat (fn [{:keys [generator-seq group-size initial-index]}]
              (map #(assoc % :parent generator-seq)
                   (all-range-permutations generator-seq (dec group-size)
                                           (inc initial-index))))
            generator-seqs))
  (all-sub-range-permutations
   [{:generator-seq [5 9 5 9 7 7 7 7 7 9], :group-size 4, :initial-index 0}
    {:generator-seq [7 5 9 5 9 7 7 7 7 9], :group-size 4, :initial-index 1}
    {:generator-seq [7 7 5 9 5 9 7 7 7 9], :group-size 4, :initial-index 2}
    {:generator-seq [7 7 7 5 9 5 9 7 7 9], :group-size 4, :initial-index 3}
    {:generator-seq [7 7 7 7 5 9 5 9 7 9], :group-size 4, :initial-index 4}]))


(do
  (defn range-permutation-params [group-size]
    (map (fn [idx]
           {:range-len (- group-size idx)
            :initial-index idx})
         (range group-size)))
  (range-permutation-params 4))
(do

  (defn all-permutations-for-base-permutation
    [{:keys [generator-seq group-size initial-index]}]
    (loop [perms [(all-range-permutations generator-seq group-size initial-index)]]
      (if (-> perms last first :group-size (> 1))
        (recur (conj perms (all-sub-range-permutations (last perms))))
        perms)))
  (all-permutations-for-base-permutation
   {:group-size 4, :generator-seq [5 9 5 9 7 7 7 7 7 9], :initial-index 0}))


;;  These next two functions will use all-range-permutations-2 see comments on that function
(defn all-sub-range-permutations-2 [scales]
  (mapcat (fn [{:keys [generator-seq group-size initial-index]}]
            (map #(assoc % :parent generator-seq)
                 (all-range-permutations-2 generator-seq (dec group-size)
                                           (inc initial-index))))
          scales))

(defn all-permutations-for-base-permutation-2
  [{:keys [generator-seq group-size initial-index]}]
  (loop [perms [(all-range-permutations-2 generator-seq group-size initial-index)]]
    (if (-> perms last first :group-size (> 1))
      (recur (conj perms (all-sub-range-permutations-2 (last perms))))
      perms)))
;; G = generator, C = closing interval, R = reciprocal interval
(do
  (defn get-reciprocal-interval
    "R+C === 2*G"
    [G C]
    (- (* 2 G) C))

  (let [G 7 C 9]
    (= (+ C (get-reciprocal-interval G C))
       (* G 2)))
  (let [G 5 C 4]
    (= (+ C (get-reciprocal-interval G C))
       (* G 2))))

(do
  ;; #dbg
  (defn base-permutations
    "`G` = generating interval
  `C` = closing interval"
    [scale-len G C]
    (let [
          ;; [min* max*] (sort [G C])
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
  #_(base-permutations 10 7 9))
(base-permutations 5 5 4)
(base-permutations 5 5 4)

(defn remove-duplicates [coll]
  (:coll (reduce (fn [{:keys [coll already-in] :as acc}
                     el]
                   (let [gen-seq (el :generator-seq)]
                     (if (already-in gen-seq) acc
                         {:coll (conj coll el)
                          :already-in (conj already-in gen-seq)})))
                 {:coll [] :already-in #{}} coll)))

(do
  (defn get-ratio-interval-sequence-2
    [scale interval]
    (->> scale
         repeat
         (apply concat)
         (partition interval)
         (map (partial apply *))
         (take (count scale))))
  (comment (get-ratio-interval-sequence-2 [9/8 9/8 10/9 16/15 9/8 9/8 256/243] 3)
           (get-ratio-interval-sequence-2 [32/27 9/8 135/128 4096/3645 9/8 135/128 16/15] 3)))


(comment
  (->> (base-permutations 10 7 9)
       (map all-permutations-for-base-permutation)
       flatten
       #_(map #(dissoc % :group-size :initial-index :parent))
       (map :generator-seq)
       remove-duplicates
       #_  clojure.pprint/pprint)

  (->> (base-permutations 7 5 6)
       (map all-permutations-for-base-permutation)
       flatten
       (map :generator-seq)
       remove-duplicates
       clojure.pprint/pprint)

  (->> (base-permutations 5 5 4)
       (map all-permutations-for-base-permutation)
       flatten
       (map :generator-seq)
       remove-duplicates
       #_   clojure.pprint/pprint)

  (into []
        (get-ratio-interval-sequence
         [10/9 9/8 9/8 16/15 10/9 9/8 16/15] 3))
  (do
    (defn intervals->scale [intervals]
      (->> intervals
           (reduce (fn [acc ivl]
                     (conj acc
                           (let [point (* (last acc) ivl)]
                             (if (> point 2)
                               (/ point 2) point))))
                   [1])
           sort
           (partition 2 1)
           (map (fn [[a b]] (/ b a)))))
    (intervals->scale [45/32 27/20 4/3 4/3 4/3 4/3 4/3]))


;;; testing the permutation algorithm with some of the cases from the xen9mar.pdf
  (->> [{:group-size 2,
         :generator-seq (into []
                      (get-ratio-interval-sequence-2
                       [9/8 9/8 9/8 256/243 9/8 9/8 256/243] 3))
         :initial-index 0}]
       (map all-permutations-for-base-permutation-2)
       flatten
       (map :generator-seq)
       remove-duplicates
       (map (fn [intervals] (intervals->scale intervals)))
       )

  (->> [{:group-size 2,
         :generator-seq (into []
                      (get-ratio-interval-sequence-2
                       [10/9 9/8 9/8 16/15 10/9 9/8 16/15] 2))
         :initial-index 0}]
       (map all-permutations-for-base-permutation-2)
       flatten
       (map :generator-seq)
       remove-duplicates
       (map (fn [intervals] (intervals->scale intervals)))
       )

  (->> [{:group-size 2, :generator-seq
         (into []
               (get-ratio-interval-sequence-2
                [32/27 9/8 135/128 4096/3645 9/8 135/128 16/15] 3))
         :initial-index 1}]
       (map all-permutations-for-base-permutation-2)
       flatten
       #_(map #(dissoc % :group-size :initial-index :parent))
       (map :generator-seq)
       remove-duplicates
       #_(remove #(not= (last %) 4/3))
       #_(map (fn [intervals] {:fourths intervals :generator-seq (intervals->scale intervals)}))
       ))




(comment
  ;;  not sure what I am doing....
  #_(defn scale->marwa-interval-sequence
      "Finds the _privileged_ sequence to use with the marwa algorithm"
      [scale]
      (map (partial get-interval-sequence scale) (range 1 (count scale))))

  (scale->marwa-interval-sequence [1 1 1 4 1 1 4])
  (scale->marwa-interval-sequence [2 3 1 2 1 2 1]))

(comment
  (get-interval-sequence [1 2 2 1 2 2 2] 3)
  (get-interval-sequence [3 2 3 2 2] 2)
  (get-interval-sequence [1 1 1 4 1 1 4] 1) ; (1 1 1 4 1 1 4)
  (get-interval-sequence [1 1 1 4 1 1 4] 2) ; (2 5 2 5 2 5 5)
  (get-interval-sequence [1 1 1 4 1 1 4] 3) ; (3 6 6 6 6 6 6)
  (get-interval-sequence [1 1 1 4 1 1 4] 4) ; (7 7 7 7 7 7 10)
  (get-interval-sequence [1 1 1 4 1 1 4] 5) ; (8 8 11 8 11 8 11)
  (get-interval-sequence [1 1 1 4 1 1 4] 6) ; (9 12 12 9 12 12 12)
  ;; Exploring cases like 7)13 where the closing interval appears
  ;; several times in the sequence






  8 8 11 8 11 8 11
  a. [5 11] 11 8 11 8 11
  b. 11 [5 11] 8 11 8 11
  c. 11 8 11 [5 11] 8 11
  a. 5 [11] 11 8 11 8 11 -- repeated
  d. 5 11 [11] 8 11 8 11
  e. 11 8 [11] 11 8 11
  f. 11 8 11 [11] 8 11
  b. 11 5 [11] 8 11 8 11 --repeated
  g. 11 5 8 [11] 11 8 11
  h. 11 5 8 11 [11] 8 11
  c. 11 8 11 5 [11] 8 11 -- repeated)

(do
  (defn interval-seq->degs [scale-size interval-seq]
    (reduce #(conj %1 (-> %1 last (or 0) (+ %2) (mod scale-size))) [] interval-seq))
  (defn degs->scale [scale-size degs]
    (->> (conj degs scale-size) sort (partition 2 1) (map (fn [[a b]] (- b a))))))


(degs->scale 12 (interval-seq->degs 12 [6 5 4 6 4 6 5]))

(degs->scale 13 (interval-seq->degs 13 [5 11 11 8 11 8 11])) ;a.

(degs->scale 13 (interval-seq->degs 13 [11 5 8 11 11 8 11])); b.

#_(degs->scale 13 (interval-seq->degs 13 [8 8 11 8 11 8 11]))

()
(degs->scale 13 (interval-seq->degs 13 '(7 7 7 7 7 7 10)))
(defn intervals->scale-2 [scale-size intervals]
  (degs->scale scale-size (interval-seq->degs scale-size intervals)))

(do
  (defn best-sequence?
    "A sequence consisting of n generators (G) and a single closing (C) interval"
    [sequence-freqs]
    (let [two-keys? (= 2 (count (keys sequence-freqs)))
          nG&1C? (contains? (set (vals sequence-freqs)) 1)]
      (and two-keys? nG&1C?)))
  (best-sequence? (frequencies '(7 7 7 7 7 6 7)))

  (defn sequence-analysis [sequence]
    (let [freqs (frequencies sequence)]
      {:generator-freqs freqs
       :best-sequence? (best-sequence? freqs)
       :sorted-generators-by-high-freq (->> freqs
                                            (sort-by second >)
                                            (map first))}))
  (sequence-analysis '(7 7 7 7 7 6 7)))

(defn get-possible-generator-sequences
  [scale]
  (map (fn [generator]
         (let [sequence (get-interval-sequence scale generator)]
           (merge {:generator generator ;; in terms of # of notes in scale
                   :sequence  sequence}
                  (sequence-analysis sequence))))
       (range 1 (count scale))))


(defn mos-permutations [scale-size base-permutations]
  (->> base-permutations
       (map all-permutations-for-base-permutation)
       flatten
       #_(map :generator-seq) ;; generator list
       remove-duplicates
       (map (fn [data]
              (assoc data :scale
                     (intervals->scale-2 scale-size
                                         (data :generator-seq))))) ;; scale list
       #_(remove #(some zero? %))
       ))

#_(coprimes 13)
(comment
  ;; APP Flow

  ;; user inputs scale
  #_(def scale [1 1 1 4 1 1 4])
  (def scale [2 2 2 3 2 2 3])
  (def scale [1 2 2 1 2 2 2])
  (def scale-size (apply + scale))
  (-> scale-size)
  ;; system gives the user a set of interval sequences to choose
  ;; ...but sometimes just one(?)
  (map (fn [generator]
         [generator
          (get-interval-sequence scale generator)])
       (range 1 (count scale)))

  ;; (7 7 7 7 7 7 10)

  (base-permutations (count scale) 7 8)
  (mos-marwa (base-permutations (count scale) 5 6))
  (->> (base-permutations (count scale) 5 6)
       (map all-permutations-for-base-permutation)
       flatten
       (map :generator-seq) ;; generator list
     #_ #_  remove-duplicates
       (map #(intervals->scale-2 scale-size %)) ;; scale list
       #_(remove #(some zero? %))
       )

  )

(comment
;;; testing the permutation algorithm with some of the cases from the xen9mar.pdf
  ;; Fig. 2
  (->> [{:group-size 2,
         :generator-seq (into []
                      (get-ratio-interval-sequence-2
                       [9/8 9/8 9/8 256/243 9/8 9/8 256/243] 3))
         :initial-index 0}]
       (map all-permutations-for-base-permutation-2)
       flatten
       (map :generator-seq)
       remove-duplicates
       #_(map (fn [intervals] (intervals->scale intervals)))
       ))
