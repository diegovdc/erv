(ns erv.mos.submos
  (:require [erv.utils.core :refer [rotate coprime?]]
            [erv.mos.mos :as mos]
            [clojure.set :as set]))


(defn get-all-rotations [pattern]
  (mapv #(into [] (rotate pattern %))
        (range (count pattern))))

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

#_(->> [5 4 5 4 4]
       get-all-rotations
       (map #(->> (group  [1 1 1 1 2 1 1 1 2 1 1 1 1 2 1 1 1 2 1 1 1 2] %)
                  (map (partial apply + ))))
       sip     (deduplicate #{})
       :mos-set
       (into []))



(defn groups->submos [groups]
  (mapv #(apply + %) groups))

(defn included? [mos-set rotations]
  (reduce #(if (mos-set %2) (reduced true) %1)
          false
          rotations))

(do
  (defn deduplicate [mos-set new-moses]
    (reduce (fn [{:keys [mos-set already-in] :as acc} mos]
              (let [rots (get-all-rotations mos)]
                (if (included? already-in rots)
                  acc
                  {:mos-set (conj mos-set mos)
                   :already-in (apply conj mos-set rots)})))
            {:mos-set mos-set :already-in mos-set}
            new-moses))
  (deduplicate #{} [[1 1 2] [1 2 1] [3 2 3]]))
(get-all-rotations [1 2 3])
(do
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
    (:mos-map
     (reduce (fn [{:keys [rotations-map included-rotations-set] :as acc} mos-data]
               (let [rots (get-all-rotations (mos-data :mos))]
                 (if (included? included-rotations-set rots)
                   (update-in acc [:mos-map (rotations-map (set rots))] conj mos-data)
                   (-> acc
                       (assoc-in [:mos-map (mos-data :mos)] [mos-data])
                       (assoc-in [:rotations-map (set rots)] (mos-data :mos))
                       (update :included-rotations-set #(apply conj % rots))))))
             {:mos-map {} :rotations-map {} :included-rotations-set #{}}
             submos-list)))
  (submos-list-by-mos '({:mos [5 5 2], :degree 0} {:mos [5 4 3], :degree 1} {:mos [5 5 2], :degree 2} {:mos [4 5 3], :degree 3} {:mos [5 5 2], :degree 4})))

(def mos-freqs (fn [mos] (-> mos frequencies vals set)))

(defn all-mos-freqs [mos generator]
  (->> (mos/make-mos (apply + mos) generator)
       (map (comp set vals frequencies))
       set))


(defn make-submos-for-pattern [mos pattern]
  (->> mos
       get-all-rotations
       (map-indexed (fn [index mos*] {:mos (groups->submos (group mos* pattern))
                                     :degree index}))))

(defn true-submos? [period generator submos]
  (let [mos (mos/make-mos period generator)
        size (count (first submos))
        mos-row  (set (get-all-rotations (first (filter #(= (count %) size) mos))))]
    (println mos-row)
    (boolean (seq (set/intersection mos-row submos)))))

(true-submos? 12 5  (set (map :mos '({:mos [5 5 2], :degree 0}
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
                                               (set (map :mos submos)))
                   :submos submos
                   :submos-by-mos (submos-list-by-mos submos)})))
         (into []))))

(defn filter-mos-members [mos-set submos]
  (let [exclusions (into #{} (mapcat get-all-rotations mos-set))]
    (remove exclusions submos)))

(defn make-all-submos
  ([mos mos-generator] (make-all-submos mos mos-generator #{}))
  ([mos mos-generator mos-to-exclude]
   (let [generators (range 1 (Math/ceil (/ (count mos) 2)))]
     (mapcat
      (fn [gen]
        (->> gen (make-submos-for-generator mos mos-generator)
             ;; NOTE not filtering so that true submos can be seen by the consumer (they are defined by containing one of the scales of the mos)
             #_(map (fn [submos-data]
                      (update submos-data :submos
                              #(filter-mos-members mos-to-exclude  %))))))
      generators))))
(make-all-submos [3 2 3 2 2] 5)
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
         #_ :mos-set))

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
  (def mos [1 1 2 1 1 2 1 1 2 1 2])

  (def grouping-mos-pattern [2 1 2 1 2 2 1])
  (group mos grouping-mos-pattern))
