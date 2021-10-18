(ns erv.neji.core
  (:require
   [erv.edo.core :as edo]
   [erv.utils.ratios :refer [ratio-proximity ratios-sequence]]))

(defn proxi-scale
  "Makes a scale by aproximating the `target-ratios` using the `ratios`
  For a proper NEJI, the `target-ratios` can be generated with the
  `edo-ratios` fn, and the `ratios` with the `ratios-octave` fn"
  ([target-ratios ratios] (proxi-scale target-ratios ratios 2))
  ([target-ratios ratios period]
   (let [scale (->> (ratio-proximity target-ratios ratios)
                    (sort-by first)
                    (map (comp first vals)))]
     {:meta {:scale :proxi,
             :size (count scale) ,
             :proxi/target-ratios target-ratios
             :proxi/ratios ratios}
      :scale (map-indexed (fn [index {:keys [ratio target-ratio diff]}]
                            {:bounded-ratio ratio
                             :bounding-period period
                             :target-ratio target-ratio
                             :diff diff})
                          scale)})))
(comment
  (proxi-scale (edo/edo-ratios 12) (ratios-sequence 19 (* 2 19))))

(defn make
  "Create a NEJI.
  TODO This is a basic (and incomplete) implementation of NEJI"
  [edo-size prime]
  (let [data (proxi-scale (edo/edo-ratios edo-size)
                          (ratios-sequence prime (* 2 prime)))]
    (update data :meta merge {:scale :neji
                              :neji/edo-size edo-size
                              :neji/prime prime})))
(comment (neji 12 19))
