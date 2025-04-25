(ns erv.lattice.v2
  (:require
   [erv.utils.core :refer [period-reduce]]
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

(comment
  (ratio->lattice-point))

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
(comment
  (get-point-data-difference
   2
   {:ratio 15/8, :numerator 15, :denominator 8, :numer-factors [3 5], :denom-factors [2 2 2], :coords {:x 40, :y -40}}
   {:ratio 2, :numerator 2, :denominator 1, :numer-factors [2], :denom-factors [], :coords {:x 0, :y 0}}
   :numer-factors))

(defn custom-connection?
  [period point-data1 point-data2 custom-edges]
  (let [r1 (->> point-data1 :ratio (period-reduce period))
        r2 (->> point-data2 :ratio (period-reduce period))]
    (custom-edges
     (period-reduce period (/ r1 r2)))))

(comment
  (custom-connection?
   2
   {:ratio 15/8
    :numerator 15
    :denominator 8
    :numer-factors [3 5]
    :denom-factors [2 2 2]
    :coords {:x 40 :y -40}}
   {:ratio 2
    :numerator 2
    :denominator 1
    :numer-factors [2]
    :denom-factors []
    :coords {:x 0 :y 0}}
   #{15/8}))

(defn make-connection
  [diff-count-set connections-set period point-data1 point-data2 custom-edges]
  #_(println period point-data1 point-data2)
  (let [diffs (partial get-point-data-difference
                       period
                       point-data1
                       point-data2)
        get-diff-count (comp #(apply + %) vals)
        num-diff  (diffs :numer-factors)
        denom-diff (diffs :denom-factors)
        diff (diff-count-set (+ (get-diff-count num-diff)
                                (get-diff-count denom-diff)))
        custom? (custom-connection? period point-data1 point-data2 custom-edges)]
    (if (or diff custom?)
      (let [points #{(:ratio point-data1)
                     (:ratio point-data2)}]
        (conj connections-set
              (with-meta points
                {:diff diff
                 :single-factor-diff? (boolean (or (when diff (<= diff 1))
                                                   custom?))
                 :custom-connection custom?
                 :num-diff num-diff
                 :denom-diff denom-diff})))
      connections-set)))

(comment
  (make-connection
   #{0 1}
   #{#{3/2 2} #{3/2 15/8}}
   2
   {:ratio 15/8
    :numerator 15
    :denominator 8
    :numer-factors [3 5]
    :denom-factors [2 2 2]
    :coords {:x 40 :y -40}}
   {:ratio 2
    :numerator 2
    :denominator 1
    :numer-factors [2]
    :denom-factors []
    :coords {:x 0 :y 0}}
   #{15/8})
  (connect-nodes
   2
   (combine-nodes
    (->> [2 3/2 15/8]
         (map #(ratio->lattice-point % base-coords))
         (into {})))
   {:custom-edges #{15/8}}))

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
  [period combined-nodes
   & {:keys [custom-edges]
      :or {custom-edges #{}}}]
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
                                              node
                                              custom-edges))
                           edges
                           ns)]
        (if
         (and (not (ref-ratio-in-ratio-edges? (:ratio ref-node) updated-edges))
              (<= max-distance (count combined-nodes)))
          (recur combined-nodes* edges #{(inc max-distance)})

          (recur (rest combined-nodes*) updated-edges #{0 1}))))))

(defn ^:export ratios->lattice-data
  "NOTE in ClojureScript `ratios` is a list of ratios as strings"
  ([base-coords ratios
    & {:keys [custom-edges period]
       :or {custom-edges #{}
            period 2}}]
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
                    (#(connect-nodes period % {:custom-edges custom-edges}))
                    (map (fn [ratios]
                           (with-meta
                             (map (comp :coords coords-data-map) ratios)
                             (meta ratios)))))]
     {:period period
      :min-x min-x
      :max-x max-x
      :min-y min-y
      :max-y max-y
      :data coords-data
      :edges edges})))

(defn swap-coords
  [coords coord-pairs]
  (reduce (fn [coords [prime1 prime2]]
            (assoc coords
                   prime1 (coords prime2)
                   prime2 (coords prime1)))
          coords
          coord-pairs))

(comment
  (ratios->lattice-data base-coords 3 [3/2 9/8 2/1])
  (ratios->lattice-data (swap-coords base-coords [[2 3]])
                        3
                        [1 3/2 9/8 2/1 3/1])
  (ratios->lattice-data base-coords '("1/1" "15/14" "5/4" "10/7" "3/2" "12/7"))
  :rcf)
