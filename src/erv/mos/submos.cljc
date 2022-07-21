(ns erv.mos.submos
  (:require [clojure.set :as set]
            [erv.mos.mos :as mos]
            [erv.utils.core :refer [get-all-rotations rotate]]))

(defn group [mos pattern]
  (loop [groups []
         mos mos
         pattern pattern]
    (let [group-size (first pattern)
          group (take group-size mos)
          mos* (drop group-size mos)]
      (if (seq mos*)
        (recur (conj groups group)
               mos*
               (rest pattern))
        (conj groups group)))))

(defn groups->submos [groups]
  (mapv #(apply + %) groups))

(defn included? [mos-set rotations]
  (reduce #(if (mos-set %2) (reduced true) %1)
          false
          rotations))

(defn deduplicate [mos-set new-moses]
  (reduce (fn [{:keys [mos-set already-in] :as acc} mos]
            (let [rots (get-all-rotations mos)]
              (if (included? already-in rots)
                acc
                {:mos-set (conj mos-set mos)
                 :already-in (apply conj mos-set rots)})))
          {:mos-set mos-set :already-in mos-set}
          new-moses))

(defn submos-list-by-mos
  "Groups the submos-list by mos (independently of the rotation)
  `submos-list`example:
  ```
  '({:mos [5 5 2], :degree 0}
    {:mos [5 4 3], :degree 1}
    {:mos [5 5 2], :degree 2}
    {:mos [4 5 3], :degree 3}
    {:mos [5 5 2], :degree 4})
  ```"
  [submos-list]
  (->> submos-list
       (reduce (fn [{:keys [rotations-map included-rotations-set] :as acc} mos-data]
                 (let [rots (get-all-rotations (mos-data :mos))]
                   (if (included? included-rotations-set rots)
                     (update-in acc [:mos-map (rotations-map (set rots))] conj mos-data)
                     (-> acc
                         (assoc-in [:mos-map (mos-data :mos)] [mos-data])
                         (assoc-in [:rotations-map (set rots)] (mos-data :mos))
                         (update :included-rotations-set #(apply conj % rots))))))
               {:mos-map {} :rotations-map {} :included-rotations-set #{}})
       :mos-map))

(comment
  (submos-list-by-mos '({:mos [5 7]
                         :degree 0
                         :mos-degrees [0 2]
                         :rotation {:mos [5 7], :degree 0, :at-zero? true}}
                        {:mos [5 7]
                         :degree 1
                         :mos-degrees [1 3]
                         :rotation {:mos [5 7], :degree 1, :at-zero? false}}
                        {:mos [5 7]
                         :degree 2
                         :mos-degrees [2 4]
                         :rotation {:mos [5 7], :degree 2, :at-zero? false}}
                        {:mos [4 8]
                         :degree 3
                         :mos-degrees [3 0]
                         :rotation {:mos [8 4], :degree 0, :at-zero? true}}
                        {:mos [5 7]
                         :degree 4
                         :mos-degrees [4 1]
                         :rotation {:mos [7 5], :degree 1, :at-zero? false}})))

(def mos-freqs (fn [mos] (-> mos frequencies vals set)))

(defn all-mos-freqs [mos generator]
  (->> (mos/make-mos (apply + mos) generator)
       (map (comp set vals frequencies))
       set))

(defn submos-as-mos-degrees [degree mos pattern]
  (let [len (count mos)]
    (->> pattern
         (reduce #(conj %1 (mod (+ %2 (last %1)) len)) [degree])
         (drop-last 1))))

(defn rotate-near-zero [submos mos-degrees]
  (let [near-zero (apply min mos-degrees)
        rotation-amount (.indexOf mos-degrees near-zero)]
    {:mos (into [] (rotate submos rotation-amount))
     :degree near-zero
     :at-zero? (zero? near-zero)}))

(defn make-submos-for-pattern [mos pattern]
  (->> mos
       get-all-rotations
       (map-indexed
        (fn [index mos*]
          (let [submos (groups->submos (group mos* pattern))
                mos-degrees (into [] (submos-as-mos-degrees index mos pattern))]
            {:mos submos
             :degree index
             ;; represent secondary mos as degrees of primary mos
             :mos-degrees mos-degrees
             :rotation (rotate-near-zero submos mos-degrees)})))))

(comment
  (make-submos-for-pattern [3 2 3 2 2] [2 3]))

(defn true-submos? [period generator submos]
  (let [submos-set (set submos)
        mos (mos/make-mos period generator)
        size (count (first submos-set))
        mos-row  (set (get-all-rotations (first (filter #(= (count %) size) mos))))]
    (boolean (seq (set/intersection mos-row submos-set)))))

(comment
  (true-submos? 12 5  (map :mos '({:mos [5 5 2], :degree 0}
                                  {:mos [5 4 3], :degree 1}
                                  {:mos [5 5 2], :degree 2}
                                  {:mos [4 5 3], :degree 3}
                                  {:mos [5 5 2], :degree 4}))))

(defn make-submos-for-generator [mos mos-generator pattern-generator]
  (let [patterns (mos/make-mos (count mos) pattern-generator)
        period (count mos)]
    (->> patterns
         (remove #(or (= 1 (count %)) (= #{1} (set %))))
         (map (fn [pattern]
                (let [submos (make-submos-for-pattern mos pattern)]
                  {:pattern pattern
                   :period period
                   :generator pattern-generator
                   :true-submos? (true-submos? (apply + mos) mos-generator
                                               (map :mos submos))
                   :submos submos
                   :submos-by-mos (submos-list-by-mos submos)})))
         (into []))))

(defn filter-mos-members [mos-set submos]
  (let [exclusions (into #{} (mapcat get-all-rotations mos-set))]
    (remove exclusions submos)))

(defn make-all-submos
  ([mos mos-generator] (make-all-submos mos mos-generator #{}))
  ([mos mos-generator mos-to-exclude]
   ;; We have enough with only the first half of the range as the second half
   ;;   mirrors the first.
   (let [generators (range 1 (Math/ceil (/ (count mos) 2)))]
     (mapcat #(make-submos-for-generator mos mos-generator %) generators))))

(comment
;;; NOTE three approaches
  ;; 1. Tanabe Cycle: rotate the mos and group it with the pattern
  (let [mos [1 2 2 1 2 2 2]
        pattern [2 1 2 1 1]]
    (->> mos
         get-all-rotations
         (map-indexed (fn [index mos] [index (groups->submos (group mos pattern))]))
         ;; NOTE may be a good idea to keep the degrees that correspond to each submos
         #_(deduplicate #{})
         #_:mos-set))

  ;; 2. Rotate the pattern and group the mos with it
  ;; (yields a retrograde version of the submoses)
  (let [mos [1 2 2 1 2 2 2]
        pattern [2 1 2 1 1]]
    (->> pattern
         get-all-rotations
         (map #(groups->submos (group mos %)))))
  ;; 3. Rotate the mos and group the pattern
  ;; (NOTE the position of the pattern in the `group` fn)
  (let [mos [1 2 2 1 2 2 2]
        pattern [2 1 2 1 1]]
    (->> mos
         get-all-rotations
         (map #(groups->submos (group pattern %))))))

(comment
  ;;  WIP: exploring if all unique submos have a rotation that aligns to the
  ;;   zeroth index of the mos.
  (make-all-submos [1 2 2 1 2 2 2] 5)
  (->> (make-all-submos [1, 1, 4, 1, 4, 1, 1, 4, 1, 4, 1, 4] 11)
       (map (fn [mos-data]
              (->> mos-data :submos-by-mos
                   (map (fn [[pattern mosi]]
                          [pattern
                           (not (empty? (filter #(-> % :rotation :at-zero?) mosi)))]))))))
  (take 10
        (make-all-submos
         [1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 5] 50 [[311] [50 261] [50 50 211] [50 50 50 161] [50 50 50 50 111] [50 50 50 50 50 61] [50 50 50 50 50 50 11] [39 11 39 11 39 11 39 11 39 11 39 11 11] [28 11 11 28 11 11 28 11 11 28 11 11 28 11 11 28 11 11 11] [17 11 11 11 17 11 11 11 17 11 11 11 17 11 11 11 17 11 11 11 17 11 11 11 11] [6 11 11 11 11 6 11 11 11 11 6 11 11 11 11 6 11 11 11 11 6 11 11 11 11 6 11 11 11 11 11] [6 6 5 6 5 6 5 6 5 6 6 5 6 5 6 5 6 5 6 6 5 6 5 6 5 6 5 6 6 5 6 5 6 5 6 5 6 6 5 6 5 6 5 6 5 6 6 5 6 5 6 5 6 5 6 5] [1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 1 5 5 1 5 5 1 5 5 1 5 5 1 5 5] [1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4 1 1 4 1 4] [1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3 1 1 1 3 1 1 3] [1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2] [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]])))
