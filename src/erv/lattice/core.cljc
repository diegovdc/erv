(ns erv.lattice.core
  #_(:require
     [clojure.math.combinatorics :as combo]
     [clojure.set :as set]
     [erv.utils.ratios :refer [analyze-ratio]]))

;; when given a scale
;; '(1 15/14 5/4 10/7 3/2 12/7)
;; factorize scale
;; find coordinates of the dots
;; figure out how to join them (this might be the trickiest thing to do, but maybe after factorization we just need to know distance-1 and connect the dots based on it)

#_(def base-coords
    {1 {:x 0 :y 0}
     2 {:x 0 :y 0}
     3 {:x 40 :y 0}
     5 {:x 0 :y 40}
     7 {:x 13 :y 11}
     11 {:x -14 :y 18}
     13 {:x -8 :y 14}})

#_(do
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

#_(do
    (defn ratio->lattice-point
      [ratio base-coords]
      (let [{:keys [numerator denominator numer-factors denom-factors]} (analyze-ratio ratio)]
        {ratio {:ratio ratio
                :numerator numerator
                :denominator denominator
                :numer-factors numer-factors
                :denom-factors denom-factors
                :coords (make-coords base-coords numer-factors denom-factors)}}))

    #_(ratio->lattice-point 10/7 base-coords))

#_(defn get-point-data-difference
    "`factor-type` should be `:numer-factors` or `:denom-factors`"
    [period point-data1 point-data2 factor-type]
    (let [set1 (->> point-data1 factor-type (remove #(= period %)) set)
          set2 (->> point-data2 factor-type (remove #(= period %)) set)]
      (set/difference (set/union set1 set2) (set/intersection set1 set2))))
#_(do
    (defn maybe-make-d1-connection
      [connections-set period point-data1 point-data2]
      (let [get-diff-count (comp count (partial get-point-data-difference
                                                period
                                                point-data1
                                                point-data2))]
        (if (= 1 (+ (get-diff-count :numer-factors)
                    (get-diff-count :denom-factors)))
          (conj connections-set #{(:ratio point-data1)
                                  (:ratio point-data2)})
          connections-set)))

    #_(maybe-make-d1-connection #{}
                                2
                                {:ratio 15/14
                                 :numerator 15
                                 :denominator 14
                                 :numer-factors [3 5]
                                 :denom-factors [2 7]
                                 :coords {:x 27, :y 29}}
                                {:ratio 10/7
                                 :numerator 10
                                 :denominator 7
                                 :numer-factors [2 5]
                                 :denom-factors [7]
                                 :coords {:x -13, :y 29}}))

#_(do
    (defn ratios->lattice-data
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
                       (mapcat (fn [ratios]
                                 (map (comp :coords coords-data-map) ratios))))]

        {:min-x min-x
         :max-x max-x
         :min-y min-y
         :max-y max-y
         :data coords-data
         :edges edges}))

    #_(ratios->lattice-data base-coords '(1 15/14 5/4 10/7 3/2 12/7)))

