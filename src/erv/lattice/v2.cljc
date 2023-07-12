(ns erv.lattice.v2
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
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

(do
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
              denominator-factors))))

(do
  (defn ratio->lattice-point
    [ratio base-coords]
    (println ratio)
    (let [{:keys [numerator denominator numer-factors denom-factors]} (analyze-ratio ratio)]
      {ratio {:ratio ratio
              :numerator numerator
              :denominator denominator
              :numer-factors numer-factors
              :denom-factors denom-factors
              :coords (make-coords base-coords numer-factors denom-factors)}}))

  (ratio->lattice-point #?(:clj 7/4 :cljs "7/4") base-coords))

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


(do
  (defn maybe-make-d1-connection
    [connections-set period point-data1 point-data2]
    (let [get-diff-count (comp #(apply + %)
                               vals
                               (partial get-point-data-difference
                                        period
                                        point-data1
                                        point-data2))]
      (println (get-diff-count :numer-factors)
               (get-diff-count :denom-factors))
      (if (#{0 1} (+ (get-diff-count :numer-factors)
                     (get-diff-count :denom-factors)))
        (conj connections-set #{(:ratio point-data1)
                                (:ratio point-data2)})
        connections-set)))

  #_(maybe-make-d1-connection #{}
                            2
                            {:ratio 3/2
                             :numerator 3
                             :denominator 2
                             :numer-factors [3]
                             :denom-factors [2]
                             :coords {:x 40, :y 0}}
                            {:ratio 9/8
                             :numerator 9
                             :denominator 8
                             :numer-factors [3 3]
                             :denom-factors [2 2 2]
                             :coords {:x 80, :y 0}}))

(do
  (defn ratios->lattice-data
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
          edges (->> (combo/combinations coords-data 2)
                     (reduce (fn [edges [p1 p2]]
                               (println p1 p2)
                               (maybe-make-d1-connection edges 2 p1 p2))
                             #{})
                     (map (fn [ratios]
                            (map (comp :coords coords-data-map) ratios))))]

      {:period 2
       :min-x min-x
       :max-x max-x
       :min-y min-y
       :max-y max-y
       :data coords-data
       :edges edges}))

  #_(ratios->lattice-data base-coords  [3/2 9/8 2/1])
  #_(ratios->lattice-data base-coords '("1/1" "15/14" "5/4" "10/7" "3/2" "12/7")))

