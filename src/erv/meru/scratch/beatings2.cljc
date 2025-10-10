(ns erv.meru.scratch.beatings2
  (:require
   [clojure.math.combinatorics :as combo]
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [decompose-ratio make-map-by-key pow prime-factors]]))

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
                                            #_(double) (* root-freq (:diff pair))))
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
  (def beat-data
    (->> (+beat-hz-by-period 2 1 #_[1 5/4 3/2 7/4]
                             #_[1
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
                                125/64]
                             [4181/4096
                              2178309/2097152
                              17/16
                              17711/16384
                              9227465/8388608
                              9/8
                              75025/65536
                              39088169/33554432
                              305/256
                              317811/262144
                              165580141/134217728
                              323/256
                              1346269/1048576
                              21/16
                              5473/4096
                              5702887/4194304
                              89/64
                              1449/1024
                              24157817/16777216
                              377/256
                              98209/65536
                              102334155/67108864
                              1597/1024
                              104005/65536
                              13/8
                              6765/4096
                              1762289/1048576
                              55/32
                              28657/16384
                              933147/524288
                              233/128
                              121393/65536
                              31622993/16777216
                              987/512
                              514229/262144
                              2/1])
         reverse
         (map :beat-freq)
         frequencies
         (sort-by second)
         reverse)))
(comment
  (->> beat-data
       (map (fn [[beats total]]
              {:beat-hz (float beats) :factors (factorize beats) :instances total}))
       (filter #(> (:instances %) 2))
       (sort-by :beat-hz)))
(do
  (defn factorize
    [n]
    (-> n decompose-ratio :numer prime-factors))

  (factorize 102334155/67108864))

(for [a [1 2 3 4]
      b [1 2 3 4]]
  [a b])
(defn- ratio-pair->beat-data
  [beat-data]
  (make-map-by-key :pair beat-data))

#_(ratio-pair->beat-data (+beat-hz-by-period 2 256 (get-beat-data [1 5/4 3/2 7/4])))
