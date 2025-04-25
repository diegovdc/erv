(ns erv.constant-structures.brute-force
  "Find constant structures by brute force"
  (:require
   [clojure.math.combinatorics :refer [combinations]]
   [erv.constant-structures.core :refer [analyze]]
   [erv.utils.conversions :as conv]
   [erv.utils.ratios :refer [ratios->scale]]))

(def scale
  [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
   {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
   {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
   {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
   {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
   {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
   {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
   {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}])

(defn make-test-ratios
  [period period-cents cents-step added-data-fn]
  (->> (range cents-step (+ period-cents cents-step) cents-step)
       (remove (fn [cents] (> cents period-cents)))
       (map conv/cents->ratio)
       (ratios->scale period)
       (map #(assoc % :added-note? true))
       (map added-data-fn)))

(defn find-1
  [scale period cent-step total-added-notes added-notes-data-fn]
  (let [added-notes* (make-test-ratios period
                                       (conv/ratio->cents period)
                                       cent-step
                                       added-notes-data-fn)
        added-notes (combinations added-notes* total-added-notes)
        _ (println "Total combinations to test: " (count added-notes))
        test-scales (map (fn [notes] (->> (apply conj scale notes)
                                          (sort-by :bounded-ratio)))
                         added-notes)

        _ (println "Total scales to test: " (count test-scales))
        constant-structures  (->> test-scales
                                  (filter (fn [scale]
                                            (->> (analyze scale)
                                                 :constant-structure?)))
                                  (map (fn [scale]
                                         (let [scale-i (map-indexed (fn [i n] (assoc n :degree i))  scale)]
                                           {:scale scale-i
                                            :added-notes (filter :added-note? scale-i)})))
                                  (group-by (comp (partial map :degree) :added-notes))
                                  (map (fn [[degs scales]] (-> scales
                                                               first
                                                               (assoc :degrees degs)))))]

    (println "Testing done!")
    {:total (count constant-structures)
     :constant-structures constant-structures}))

(comment
  (find-1 scale 3 100 4 (fn [n] (assoc n  :color [255 0 0])))
  (require '[erv.constant-structures.graphics :refer [init-cs-tool! update-state]])
  (def brute-force-results (find-1 scale 3 100 4 (fn [n] (assoc n  :color [255 0 0]))))
  (def s (init-cs-tool! scale
                        []))
  (-> brute-force-results
      :constant-structures
      (nth 0)
      :added-notes
      (->> (map :bounded-ratio)))
  (update-state s
                scale
                (-> brute-force-results
                    :constant-structures
                    (nth 0)
                    :added-notes
                    (->> (map :bounded-ratio)))))

(defn cs-subsets
  [min-scale-size scale]
  (->>  (range min-scale-size (count scale))
        (mapcat (fn [i] (combinations scale i)))
        (filter (fn [scale]
                  (->> (analyze scale)
                       :constant-structure?)))))
