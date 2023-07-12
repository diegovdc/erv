(ns erv.scratch
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.utils.core :refer [prime-factors]]))

(defn over-next-period
  "For harmonic -ordinary- cps"
  [period target n]
  (loop [n n]
    (if (> n target) n (recur (* period n)))))

(defn over-next-period-steps
  "For melodic cps (steps in scale)"
  [period target n]
  (loop [n n]
    (if (> n target) n (recur (+ period n)))))
;; inversions of a dekany


(defn simplify-inversion [inversion]
  (let [common-factors (map (comp frequencies prime-factors) inversion)
        common-denominator (->> common-factors
                                (reduce (fn [acc m]
                                          (reduce (fn [acc [k v]]
                                                    (update acc k (fnil conj []) v)) acc m)) {})
                                (filter #(= (count common-factors)
                                            (count (second %))))
                                (map (fn [[n factor-counts]]
                                       (* n (apply min factor-counts))))
                                (apply *))]
    (mapv #(/ % common-denominator) inversion)))

(comment
  (simplify-inversion [10 18 32 56 96]))

(defn make-seed->seed-name [seed]
  (->> (count seed)
       range
       (map-indexed (fn [i n] {(nth seed i) (str (char (+ 65 i)))}))
       (apply merge)))

(defn get-cps-set-inversions
  ([{:keys [process-inversion-fn cps-set-vector next-period-fn]}]
   (let [seed->seed-name (make-seed->seed-name cps-set-vector)]
     (->> (combo/permutations cps-set-vector)
          (mapv (fn [p]
                  (let [inversion (reduce (fn [acc n]
                                            (conj acc (if (> (last acc) n)
                                                        (next-period-fn (last acc) n)
                                                        n)))
                                          [(first p)]
                                          (rest p))]
                    [(map first (sort-by second (map vector (mapv seed->seed-name p)
                                                     (process-inversion-fn inversion))))
                     (process-inversion-fn inversion)])))))))
(comment
  (get-cps-set-inversions {:cps-set-vector [1 3 7 9 11]
                           :process-inversion-fn simplify-inversion
                           :next-period-fn (partial over-next-period 2)}))

;;;;;; Some calculation for K.G.

(def combinations-columns
  ["A+B" "A+C" "A+D" "A+E" "B+C" "B+D" "B+E" "C+D" "C+E" "D+E"])

(def seed-name-columns
  ["A" "B" "C" "D" "E"])

(defn make-csv-row [[names seed]]
  (let [seed->seed-name (->> (interleave names seed)
                             (partition 2)
                             (map (comp vec reverse))
                             (into {}))
        seed-name->seed (set/map-invert seed->seed-name)
        data (->> (combo/combinations seed 2)
                  (map-indexed (fn [i c]
                                 (let [c* (sort c)]
                                   {:combination-steps c*
                                    :combination-name (apply format "%s+%s"
                                                             (sort [(seed->seed-name (first c*))
                                                                    (seed->seed-name (second c*))]))
                                    :step (apply + c*)})))
                  (reduce #(assoc %1 (:combination-name %2) %2) {}))
        steps (map (comp :step #(get data %)) combinations-columns)]
    (concat
     (map seed->seed-name seed)
     (map seed-name->seed seed-name-columns)
     steps
     [(- (apply max steps) (apply min steps))])))

(comment (map make-csv-row '([["A" "B" "C" "E" "D"] [1 3 7 11 9]])))

(defn make-inversions-csv [cps-vector]
  (spit (str (str/join "-" cps-vector) "-dekany-inversions.csv")
        (str/join "\n" (concat [(str ";;;;;"
                                     (str/join ";" seed-name-columns)
                                     ";"
                                     (str/join ";" combinations-columns)
                                     ";Range")]
                               (->> (get-cps-set-inversions {:cps-set-vector cps-vector
                                                             :process-inversion-fn identity
                                                             :next-period-fn (partial over-next-period-steps 12)})
                                    (map make-csv-row)
                                    (sort-by last)
                                    (map #(str/join ";" %)))))))

(comment
  (->> (get-cps-set-inversions {:cps-set-vector [0 1 4 6 8]
                                :process-inversion-fn identity
                                :next-period-fn (partial over-next-period-steps 12)})
       (map make-csv-row)
       (sort-by last)
       (map #(str/join ";" %)))

  (make-inversions-csv [0 1 2 4 7])

  (get-cps-set-inversions {:cps-set-vector [0 1 4 6 8]
                           :process-inversion-fn identity
                           :next-period-fn (partial over-next-period-steps 12)}))

