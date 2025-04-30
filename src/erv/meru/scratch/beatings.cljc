(ns erv.meru.scratch.beatings
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.utils.conversions :refer [cents->ratio]]))

(comment)
#_(def c (* 3 11 8))

(def c 256 #_(* 3 11 8))

(defn get-beat-data
  [root ratios]
  (->> ratios
       (mapcat
         (fn [degree ratio]
           (map (fn [i] {:degree degree
                         :ratio ratio
                         :partial i
                         :partial-ratio (* i ratio)})
                (range 1 9)))
         (range))
       #_sort
       (#(combo/combinations % 2))
       (remove (fn [[x1 x2]] (= (:ratio x1) (:ratio x2))))
       (map (fn [pair] {:pair pair
                        :diff (abs (- (:partial-ratio (first pair))
                                      (:partial-ratio (second pair))))}))
       (sort-by :diff)
       (map (fn [pair]
              (assoc pair
                     :diff-c4 (double (* root (:diff pair)))
                     :diff-c3 (double (/ (* root (:diff pair))
                                         2))
                     :diff-c2 (double (/ (* root (:diff pair))
                                         4))
                     :diff-c1 (double (/ (* root (:diff pair))
                                         8)))))))

(def metameantone-beatings
  (get-beat-data c [1
                    67/64
                    279/256
                    9/8
                    75/64
                    39/32
                    5/4
                    167/128
                    87/64
                    45/32
                    187/128
                    3/2
                    25/16
                    417/256
                    27/16
                    7/4
                    233/128
                    15/8
                    125/64]))

(->> metameantone-beatings
     (map (juxt :diff :diff-c2)))

(->> metameantone-beatings
     (map :diff)
     (remove zero?)
     (dedupe)
     (map #(/ % (* 1/2 1/128))))

(def metaslendro-beatings
  (get-beat-data c
                 [1
                  65/64
                  265/256
                  1081/1024
                  9/8
                  37/32
                  151/128
                  77/64
                  21/16
                  43/32
                  351/256
                  3/2
                  49/32
                  25/16
                  51/32
                  7/4
                  57/32
                  465/256]))
  
(->> metameantone-beatings
     #_(map :diff-c4)
     #_(remove zero?)
     #_(dedupe)
     #_(map #(/ % (* 1/2 1/128))))

(->> metaslendro-beatings
     (group-by :diff)
     #_(map :diff)
     #_(remove zero?)
     #_(dedupe)
     #_(map #(/ % (* 1/16 1/64))))


(->> metaslendro-beatings
     #_(map (juxt :diff :diff-c4))
     #_(dedupe)
     (filter #(->> % :pair (map :ratio) set ((fn [%] (% 1)))))
     )
