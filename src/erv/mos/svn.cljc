(ns erv.mos.svn
  "A pedestrian implementation to find strict x x FIXME"
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(let [model [2 3 1 3 2 3 1]
      target-size 50]
  (let [freqs (frequencies model)
        total-freqs (count freqs)
        freqs-permutations (combo/permutations (vals freqs))
        possible-interval-sizes (combo/combinations
                                 (range 1 (inc target-size))
                                 total-freqs)]
    (->> possible-interval-sizes
         (map
          (fn [intervals-sizes]
            (for [fs freqs-permutations
                  :let [res {:intervals intervals-sizes
                             :freqs fs}]
                  :when (= target-size
                           (apply + (map (fn [i f] (* i f))
                                         (:intervals res)
                                         (:freqs res))))]
              res)))
         (filter seq)
         flatten
         (map (fn [{:keys [intervals freqs]}]
                (into {} (map (fn [i f] {i f}) intervals freqs))
                ;; TODO lost some information, so need to map the output above which maps new intervals to freqs
                ;; into the structure freqs map above so that we can substitute the model's intervals with the new intervals
                )))))
