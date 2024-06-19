(ns erv.edo.core)

(defn edo-ratios
  ([divisions] (edo-ratios divisions 2))
  ([divisions bounding-ratio]
   (map #(Math/pow (Math/exp (/ % divisions)) (Math/log bounding-ratio))
        (range 0 divisions))))

(defn pattern->degrees [pattern]
  (->> pattern (drop-last 1) (reduce #(conj %1 (+ (last %1) %2)) [0])))

(comment
  (require
   '[erv.cps.core :as cps]
   '[erv.utils.conversions :as conv]
   '[erv.scale.core :refer [demo!]]
   '[erv.mos.submos :refer [make-all-submos]]
   '[erv.mos.mos :refer [make-mos]])

  (map conv/ratio->cents (edo-ratios (* 2 3 5 7)))

  (def mos (make-mos 31 5))
  (-> mos)
  (def submosi (make-all-submos (mos 6) 5))
  (do)
  (def submos) (-> submosi #_(->> (filter :true-submos?))
                   #_ #_ (nth 0) :submos
                   #_ #_           (nth 1) :mos)
  (demo! (:scale (from-pattern submos)) :note-dur 200 :direction :down)
  (demo! (:scale (from-pattern [3,5,2,5,3,5,4])) :note-dur 200 :direction :down)
  (demo! (:scale (from-pattern[6, 3, 4, 3, 7, 4, 3, 1] 2)) :note-dur 200 :direction :up ))

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


(from-pattern [ 2, 2, 5, 2, 5, 2, 5, 2, 2, 5, 2, 5, 2, 5, 2, 5])
