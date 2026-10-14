(ns erv.edo.core)

(defn edo-ratios
  ([divisions] (edo-ratios divisions 2))
  ([divisions bounding-ratio]
   (map #(Math/pow (Math/exp (/ % divisions)) (Math/log bounding-ratio))
        (range 0 divisions))))

(defn pattern->degrees [pattern]
  (->> pattern (drop-last 1) (reduce #(conj %1 (+ (last %1) %2)) [0])))

(defn from-pattern
  "For use with `mos` patterns or other custom intervalic patterns, i.e. [3 2 3 2 2]"
  ([pattern] (from-pattern pattern 2))
  ([pattern period]
   (let [divisions (apply + pattern)
         edo (edo-ratios divisions period)
         degrees (pattern->degrees pattern)]
     {:meta {:edo/pattern pattern
             :edo/divisions divisions
             :edo/period period}
      :scale (map-indexed (fn [index degree]
                            {:edo/original-degree degree
                             :edo/degree index
                             :bounded-ratio (nth edo degree)
                             :bounding-period period})
                          degrees)})))
