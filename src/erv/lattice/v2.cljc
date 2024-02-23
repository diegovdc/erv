(ns erv.lattice.v2
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.utils.ratios :refer [analyze-ratio]]))

(def base-coords
  {1 {:x 0 :y 0}
   2 {:x 0 :y 0}
   3 {:x 40 :y 0}
   5 {:x 0 :y -40}
   7 {:x 13 :y -11}
   11 {:x -14 :y -18}
   13 {:x -8 :y -4}
   17 {:x -5 :y -32}
   19 {:x 7 :y -25}
   23 {:x 20 :y -6}})

(defn make-coords [base-coords numerator-factors denominator-factors]
  (let [numer-coords (reduce (fn [{:keys [x y]} factor]
                               {:x (+ x (get-in base-coords [factor :x]))
                                :y (+ y (get-in base-coords [factor :y]))})
                             {:x 0 :y 0}
                             numerator-factors)]
    (reduce (fn [{:keys [x y]} factor]
              {:x (- x (get-in base-coords [factor :x]))
               :y (- y (get-in base-coords [factor :y]))})
            numer-coords
            denominator-factors)))

(defn ratio->lattice-point
  [ratio base-coords]
  (let [{:keys [numerator denominator numer-factors denom-factors]} (analyze-ratio ratio)]
    {ratio {:ratio ratio
            :numerator numerator
            :denominator denominator
            :numer-factors numer-factors
            :denom-factors denom-factors
            :coords (make-coords base-coords numer-factors denom-factors)}}))

(defn get-point-data-difference
  "`factor-type` should be `:numer-factors` or `:denom-factors`"
  [period point-data1 point-data2 factor-type]
  (let [ratio-freqs1 (->> point-data1 factor-type (remove #(= period %)) frequencies)
        ratio-freqs2 (->> point-data2 factor-type (remove #(= period %)) frequencies)
        factors-set (set (keys (merge ratio-freqs1 ratio-freqs2)))]
    (reduce (fn [acc factor]
              (assoc acc factor (#?(:clj Math/abs :cljs js/Math.abs)
                                 (- (ratio-freqs1 factor 0)
                                    (ratio-freqs2 factor 0)))))
            {}
            factors-set)))

(defn make-connection
  [diff-count-set connections-set period point-data1 point-data2]
  (let [get-diff-count (comp #(apply + %)
                             vals
                             (partial get-point-data-difference
                                      period
                                      point-data1
                                      point-data2))]
    (if (diff-count-set (+ (get-diff-count :numer-factors)
                           (get-diff-count :denom-factors)))
      (conj connections-set #{(:ratio point-data1)
                              (:ratio point-data2)})
      connections-set)))

(defn combine-nodes [coords-data]
  (->> (for [[r1 d1] coords-data
             [r2 d2] coords-data]
         (when-not (= r1 r2)
           [d1 d2]))
       (remove nil?)
       (reduce (fn [acc [r d]]
                 (update acc r conj d))
               {})))

(defn ref-ratio-in-ratio-edges?
  [ref-ratio ratio-edges]
  (->> ratio-edges
       (filter #(% ref-ratio))
       first
       boolean))

(defn connect-nodes
  [period combined-nodes]
  (loop [combined-nodes* combined-nodes
         edges #{}
         distances-set #{0 1}]
    (if-not (seq combined-nodes*)
      edges
      (let [[ref-node ns] (first combined-nodes*)
            max-distance (apply max distances-set)
            updated-edges (reduce
                           (fn [edges* node]
                             (make-connection distances-set
                                              edges*
                                              period
                                              ref-node
                                              node))
                           edges
                           ns)]
        (if
         (and (not (ref-ratio-in-ratio-edges? (:ratio ref-node) updated-edges))
              (<= max-distance (count combined-nodes)))
          (recur combined-nodes* edges #{(inc max-distance)})

          (recur (rest combined-nodes*) updated-edges #{0 1}))))))

(defn ^:export ratios->lattice-data
  "NOTE in ClojureScript `ratios` is a list of ratios as strings"
  [base-coords ratios]
  (let [coords-data-map (->> ratios
                             (map #(ratio->lattice-point % base-coords))
                             (into {}))
        coords-data (vals coords-data-map)
        coords (->> coords-data
                    (map :coords))
        min-x (->> coords (map :x) (apply min))
        max-x (->> coords (map :x) (apply max))
        min-y (->> coords (map :y) (apply min))
        max-y (->> coords (map :y) (apply max))
        edges (->> coords-data-map
                   combine-nodes
                   (connect-nodes 2)
                   (map (fn [ratios]
                          (map (comp :coords coords-data-map) ratios))))]
    {:period 2
     :min-x min-x
     :max-x max-x
     :min-y min-y
     :max-y max-y
     :data coords-data
     :edges edges}))

#_(ratios->lattice-data base-coords '("1/1" "15/14" "5/4" "10/7" "3/2" "12/7"))
