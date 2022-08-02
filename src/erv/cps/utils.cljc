(ns erv.cps.utils)

;; implement correctly
#_(defn get-cps-diads [cps-scale]
    (->> (combo/combinations cps-scale 2)
         (filter (fn [[a b]]
                   (seq (set/intersection (:archi-set a) (:archi-set b)))))
         (map (fn [[a b]] [(:bounded-ratio a) (:bounded-ratio b)]))))
