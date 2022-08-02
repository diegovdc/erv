(ns erv.edo.cps
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.neji.core :refer [proxi-scale]]
   [erv.utils.conversions :as convo]
   [erv.utils.core :refer [indexes-of]]))

(defn get-cps-diads [cps-scale]
  (->> (combo/combinations cps-scale 2)
       (filter (fn [[a b]]
                 (seq (set/intersection (:archi-set a) (:archi-set b)))))
       (map (fn [[a b]] [(:bounded-ratio a) (:bounded-ratio b)]))))

(get-cps-diads (:scale (cps/make 2 [1 3 5 7] :norm-fac 7)))

(map convo/ratio->cents (edo/edo-ratios 31))

(let [edo-31 (edo/edo-ratios 22)
      cps-scale (:scale (let [set* [1 3 7 9 11 15]]
                          (cps/make 3 set*
                                    :norm-fac (apply * (take 3 set*)))))
      tempered-cps (proxi-scale (map :bounded-ratio cps-scale)
                                edo-31)]
  (->> tempered-cps
       :scale
       (map (fn [n]
              (assoc n :edo-31-index (first (indexes-of (:bounded-ratio n) edo-31)))))
       #_(map :edo-31-index)
       (map :diff-cents)))

(comment
  ;; in 31 edo
  ;; (cps/make 2 [1 7 3 5 23] :norm-fac 23) =>  (0 2 9 10 12 18 19 25 25 27)
  )

