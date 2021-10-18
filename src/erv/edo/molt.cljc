(ns erv.edo.molt
  "Modes of limited transposition"
  (:require [erv.utils.core :as utils]
            [clojure.math.combinatorics :as combo]))

(do
  (defn motl-for-factor
    ([edo-size factor] (motl-for-factor edo-size factor false))
    ([edo-size factor permutations?]
     (let [unit-reps (/ edo-size factor)
           get-permutations (if permutations?
                              #(mapcat combo/permutations %)
                              identity)]
       (->> (range 1 factor)
            (mapcat #(repeat (- factor %) %))
            (concat (list factor))
            combo/subsets
            (filter #(= factor (apply + %)))
            get-permutations
            (map #(flatten (repeat unit-reps %)))))))
  #_(motl-for-factor 12 3))



(defn partition-factor [factor]
  (->> (combo/partitions (repeat factor 1) :min 1 :max (dec factor))
       (map (partial map (partial apply +)))))

(do
  (defn dedupe-permutations [perms]
    (:perms
     (reduce
      (fn [{:keys [perms perms-rotations] :as acc} perm]
        (if (perms-rotations perm)
          acc
          {:perms (conj perms perm)
           :perms-rotations (into perms-rotations
                                  (utils/get-all-rotations perm))}))
      {:perms () :perms-rotations #{}}
      perms)))

  (defn segment-permutations [factor-partition]
    (let [freqs (frequencies factor-partition)
          dont-attempt? (and (= 2 (count freqs))
                             (-> freqs vals set (contains? 1)))]
      (if dont-attempt?
        (list (vec factor-partition))
        (->> (combo/permutations factor-partition)
             dedupe-permutations))))
  #_(segment-permutations '(1 1 1 2)))

(do
  (defn segments
    [edo-size & {:keys [permutations? factors]
                 :or {permutations? true}}]
    (let [get-perms (if permutations? segment-permutations identity)]
      (->> (or factors (utils/factors edo-size))
           (mapcat #(partition-factor %))
           (mapcat get-perms))))

  #_(segments 210 :permutations? true))

(defn make [edo-size & {:keys [permutations? factors]
                        :or {permutations? true}} ]
  (->> (segments edo-size
                 :permutations? permutations?
                 :factors factors)
       (map (fn [seg]
              (let [reps (/ edo-size (apply + seg))]
                (flatten (repeat reps seg)))))
       set))

(defn from-segments [edo-size segments]
  (map (fn [seg]
         (let [reps (/ edo-size (apply + seg))]
           (flatten (repeat reps seg))    ))
       segments))

(from-segments 12 [[1 2] [1 1 1 3]])

(comment
  ;; ways to explore motl in heigher edos
  ;; (trying to avoid too many options to compute)

  ;;  #1
  (def edo-size 28)
  (def segs (segments edo-size :permutations? true))
  (->> segs
       (filter #(< (count %) 5))
       (filter #(let [min* (apply min %) max* (apply max %)]
                  (and (>= min* 2) #_(> 8 (- max* min*)))))
       (take 200)
       (from-segments edo-size))
  (utils/factors 54)
  (count (partition-factor 27))

  ;;  # 2 -- more performant
  (->> (partition-factor 27)
       (filter #(< (count %) 5))
       (filter #(let [min* (apply min %) max* (apply max %)]
                  (and (>= min* 4) (> 8 (- max* min*)))))
       (take 200))
  (make 22))

(comment
  ;; WIP doesn't really work

  (do
    (defn from-chord [edo-size factor chord]
      (->> (range 0 edo-size factor)
           (mapcat
            (fn [degree] (map
                         (fn [interval] (mod (+ degree interval) edo-size))
                         chord)))
           sort
           dedupe
           (partition 2 1)
           (map (fn [[a b]] (- b a)))
           ))
    (from-chord 24 8 [0 7 14 18])))
