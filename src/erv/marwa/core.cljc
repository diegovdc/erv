(ns erv.marwa.core
  (:require [clojure.string :as str]
            [erv.mos.mos :as mos]))

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
         (partition interval 1)
         (map (partial apply +))
         (take (count scale))))         ; only take the intervals before the cycle repeats
  (get-interval-sequence kalayan 3))


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
         (map (fn [scale] {:scale scale
                          :fourths (get-interval-sequence scale generator)}))
         (#(conj % {:scale scale :fourths (get-interval-sequence scale generator)}))))
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
    (let [perms-start-indexes (-> (count scale) (- range-len 2 initial-index)
                                  range
                                  (->> (map (partial + initial-index))))]
      (reduce (fn [acc idx]
                (let [prev-scale (:scale (last acc))
                      scale  (permutate-range-fwd prev-scale idx (+ idx range-len))]
                  (conj acc {:scale scale
                             :group-size range-len
                             :initial-index (inc idx)
                             }))
                )
              [{:scale scale
                :group-size range-len
                :initial-index initial-index}]
              perms-start-indexes)))
  (all-range-permutations [5 9 5 9 7 7 7 7 7 9] 4 0))


;;; I think this is DONE
;;; TODO WIP `all-range-permutations` now generates all the main blocks,
;;; but for example, the generated permutations of group-size 3
;;; should then used as a base for permutations of size 2 and size 1,
;;; the ones of size 4, should have size 3, 2 and 1, etc...


(do
  (defn all-sub-range-permutations [scales]
    (mapcat (fn [{:keys [scale group-size initial-index]}]
              (map #(assoc % :parent scale)
                   (all-range-permutations scale (dec group-size)
                                           (inc initial-index))))
            scales))
  (all-sub-range-permutations
   [{:scale [5 9 5 9 7 7 7 7 7 9], :group-size 4, :initial-index 0}
    {:scale [7 5 9 5 9 7 7 7 7 9], :group-size 4, :initial-index 1}
    {:scale [7 7 5 9 5 9 7 7 7 9], :group-size 4, :initial-index 2}
    {:scale [7 7 7 5 9 5 9 7 7 9], :group-size 4, :initial-index 3}
    {:scale [7 7 7 7 5 9 5 9 7 9], :group-size 4, :initial-index 4}]))


(do
  (defn range-permutation-params [group-size]
    (map (fn [idx]
           {:range-len (- group-size idx)
            :initial-index idx})
         (range group-size)))
  (range-permutation-params 4))
(do

  (defn all-permutations-for-base-permutation
    [{:keys [scale group-size initial-index]}]
    (loop [perms [(all-range-permutations scale group-size initial-index)]]
      (if (-> perms last first :group-size (> 1))
        (recur (conj perms (all-sub-range-permutations (last perms))))
        perms)))
  (all-permutations-for-base-permutation
   {:group-size 4, :scale [5 9 5 9 7 7 7 7 7 9], :initial-index 0}))

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
  (defn base-permutations [scale-len G C]
    (let [
          ;; [min* max*] (sort [G C])
          ;; R (- min* (- max* min*))
          R (get-reciprocal-interval G C)
          group [R C]
          max-groups (quot scale-len 2)]

      (map (fn [total]
             {:group-size (* 2 total)
              :scale
              (into [] (flatten
                        [(repeat total group)
                         (repeat (- scale-len 1 (* 2 total)) G)
                         C]))
              :initial-index 0})
           (range 1 max-groups))))
  #_(base-permutations 10 7 9))
(base-permutations 5 5 4)

(defn remove-duplicates [coll]
  (:coll (reduce (fn [{:keys [coll already-in] :as acc}
                     el]
                   (if (already-in el) acc
                       {:coll (conj coll el)
                        :already-in (conj already-in el)}))
                 {:coll [] :already-in #{}} coll)))

(base-permutations 10 7 9)
(comment
  (->> (base-permutations 10 7 9)
       (map all-permutations-for-base-permutation)
       flatten
       #_(map #(dissoc % :group-size :initial-index :parent))
       (map :scale)
       remove-duplicates
       clojure.pprint/pprint)

telete

  (->> (base-permutations 7 5 6)
       (map all-permutations-for-base-permutation)
       flatten
       #_(map #(dissoc % :group-size :initial-index :parent))
       (map :scale)
       remove-duplicates
       clojure.pprint/pprint)

  (->> (base-permutations 5 5 4)
       (map all-permutations-for-base-permutation)
       flatten
       #_(map #(dissoc % :group-size :initial-index :parent))
       (map :scale)
       remove-duplicates
       clojure.pprint/pprint))

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
