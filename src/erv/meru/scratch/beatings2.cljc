(ns erv.meru.scratch.beatings2
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [make-map-by-key pow]]))

(comment)
#_(def c (* 3 11 8))

(def c 256 #_(* 3 11 8))

(do
  (defn get-beat-data
    ([ratios] (get-beat-data (range 1 9) ratios))
    ([partials ratios]
     (->> ratios
          (mapcat
           (fn [degree ratio]
             (map (fn [i] {:degree degree
                           :ratio ratio
                           :partial i
                           :partial*ratio (* i ratio)})
                  partials))
           (range))
          (#(combo/combinations % 2))
          (remove (fn [[x1 x2]] (= (:ratio x1) (:ratio x2))))
          (map (fn [pair] {:pair pair
                           :diff (abs (- (:partial*ratio (first pair))
                                         (:partial*ratio (second pair))))}))
          (sort-by :diff)
          #_(map (fn [pair]
                   (assoc pair
                          :diff-c4 (double (* root (:diff pair)))
                          :diff-c3 (double (/ (* root (:diff pair))
                                              2))
                          :diff-c2 (double (/ (* root (:diff pair))
                                              4))
                          :diff-c1 (double (/ (* root (:diff pair))
                                              8))))))))

  (def metameantone-beatings)
  (get-beat-data [1
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
                  125/64])
  (get-beat-data [1
                  5/4
                  3/2
                  7/4]))

(do
  (defn- get-root-note-lowest-freq
    [period freq]
    (let [lowest-freq 20]
      (loop
       [freq freq]
        (cond
          (< freq lowest-freq) (recur (* freq period))
          (>= freq (* lowest-freq period)) (recur (/ freq period))
          (and (>= freq lowest-freq) (> (* lowest-freq period) freq)) freq))))
  (get-root-note-lowest-freq 2 20)
  (get-root-note-lowest-freq 2 40)
  (get-root-note-lowest-freq 2 16))

(do
  (defn- get-freq-periods-range
    [period initial-freq max-freq]
    (->> (range)
         (map #(* initial-freq (pow period %)))
         (take-while #(<= % max-freq))))
  (get-freq-periods-range 2 1 4000))

(do
  ;; TODO refactor to smaller functions and rename
  (defn +beat-hz-by-period
    "Using a root note, map the beat-data over the a range from 20 to 4000hz"
    [period root ratios]
    (let [max-freq 8000
          beat-data (get-beat-data ratios)
          lowest-root (get-root-note-lowest-freq period root)
          root-periods-freqs (get-freq-periods-range period lowest-root max-freq)
          pair->beat-data (->> beat-data
                               (mapv
                                (fn [pair]
                                  (->> root-periods-freqs
                                       (mapv
                                        (fn [period root-freq]
                                          (let [k (keyword (str "diff-period-" period))]
                                            (double (* root-freq (:diff pair)))))
                                        (range))
                                       #_(into {})
                                       (assoc pair
                                              :root-hz lowest-root
                                              :beat-hz-by-period))))
                               (group-by  (comp set #(map :ratio %) :pair))
                               (mapv (fn [[k v]]
                                       [k (sort-by (juxt
                                                    (comp :partial first :pair)
                                                    (comp :partial second :pair))
                                                   v)]))
                               (into {})
                               #_(make-map-by-key  (comp set #(map :ratio %) :pair)))
          ratio-pairs (->> (keys pair->beat-data)
                           (sort-by (juxt first second)))]
      (mapcat
       (fn [period root]
         (mapcat
          (fn [pair]
            (->> (pair->beat-data pair)
                 (keep (fn [beat-data]
                         (let [data ((juxt (comp :partial first :pair)
                                           (comp :partial second :pair)
                                           :beat-hz-by-period)
                                     beat-data)
                               [pr1 pr2 beats-by-period] data
                               beats (nth beats-by-period period)
                               [r1 r2] (sort pair)]
                           (when (and (< beats 20)
                                      (not (zero? beats)))
                             {:period period
                              :root-hz root
                              :root (cps->name* root)
                              :ratio-1 r1
                              :ratio-2 r2
                              :ratio-1-partial pr1
                              :ratio-2-partial pr2
                              :beat-freq beats}))))))
          ratio-pairs))
       (range)
       root-periods-freqs)))
  (->> (+beat-hz-by-period 2 1 #_[1 5/4 3/2 7/4]
                           [1
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
                            125/64])))

(for [a [1 2 3 4]
      b [1 2 3 4]]
  [a b])
(defn- ratio-pair->beat-data
  [beat-data]
  (make-map-by-key :pair beat-data))

#_(ratio-pair->beat-data (+beat-hz-by-period 2 256 (get-beat-data [1 5/4 3/2 7/4])))
