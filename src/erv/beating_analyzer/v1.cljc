(ns erv.beating-analyzer.v1
  #?(:cljs (:refer-clojure :exclude [>= <= > < + -  * / -  numerator denominator integer?
                                     mod rem quot even? odd? abs]))

  (:require
   [clojure.core :as core]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [erv.utils.exact :as exact.utils]
   #?(:cljs [com.gfredericks.exact :as e :refer [>= <= > < + -  * / - abs]])
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [decompose-ratio factorize-ratio factors->hiccup
                           make-map-by-key pow prime-factors]]
   [erv.utils.ratios :refer [ratios->scale-data]]
   [taoensso.timbre :as timbre]
   #?(:clj [hiccup2.core :as h])))

(def ^:private default-partials (map exact.utils/->exact (range 1 9)))

(defn get-beat-data-by-pairs
  ([ratios] (get-beat-data-by-pairs default-partials ratios))
  ([partials ratios]
   (->> ratios
        (mapcat
         (fn [degree ratio]
           (map (fn [i] {:degree degree
                         :ratio ratio
                         :partial i
                         :partial*ratio (* i ratio)})
                (map exact.utils/->exact partials)))
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

(comment
  (->> (get-beat-data-by-pairs [1 2 3 4 5 6] [1 5/4 3/2])
       (map :pair)
       (filter (fn [[a b]]
                 (and (= 0 (:degree a))
                      (= 2 (:degree b))
                      (= 3 (:partial a))
                      (= 2 (:partial b))))))

  (require '[erv.utils.exact :refer [parse-ratios]]
           '[erv.utils.exact :as exact.utils])
  (def ratios (parse-ratios "1
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
     125/64"))
  (take 10 (get-beat-data-by-pairs ratios))
  (exact.utils/->exact period))

(def ^:private lowest-freq (exact.utils/->exact 20))

(defn- get-root-note-lowest-freq
  [period freq]
  (loop
   [freq freq]
    (cond
      (< freq lowest-freq) (recur (* freq period))
      (>= freq (* lowest-freq period)) (recur (/ freq period))
      (and (>= freq lowest-freq) (> (* lowest-freq period) freq)) freq)))

(comment
  (get-root-note-lowest-freq (exact.utils/->exact 2)
                             (exact.utils/->exact 1)))

(defn- get-freq-periods-range
  [period initial-freq max-freq]
  (->> (range)
       (map #(* initial-freq (pow period %)))
       (take-while #(<= % max-freq))))

(def ^:private max-freq (exact.utils/->exact 4000))
(def ^:private max-beat-freq (exact.utils/->exact 20))

(comment
  (require '[erv.utils.core :refer [interval]]
           '[erv.utils.conversions :as conv])
  (conv/ratio->cents (interval 187/128 7/4))
  (conv/ratio->cents 6/5)
  (get-beat-data-by-pairs (range 1 7) [5/4]))

(defn get-beat-data
  "Using a root note, map the beat-data over the a range from 20 to 4000hz"
  [period root partials ratios]
  (let [beat-data (get-beat-data-by-pairs partials ratios)
        lowest-root (get-root-note-lowest-freq period root)
        root-periods-freqs (get-freq-periods-range period lowest-root max-freq)
        pair->beat-data (->> beat-data
                             (map
                              (fn [pair]
                                (->> root-periods-freqs
                                     (map
                                      (fn [period root-freq]
                                        (let [k (keyword (str "diff-period-" period))]
                                            ;; (println (:diff pair))
                                          #_(double) (* root-freq (:diff pair))))
                                      (range))
                                     #_(into {})
                                     (assoc pair
                                            :root-hz lowest-root
                                            :beat-hz-by-period))))
                             (group-by  (comp set #(map :ratio %) :pair))
                             (map (fn [[k v]]
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
                       (let [data ((juxt (comp (juxt :partial :degree) first :pair)
                                         (comp (juxt :partial :degree) second :pair)
                                         :beat-hz-by-period)
                                   beat-data)
                             [[pr1 deg1] [pr2 deg2] beats-by-period] data
                             beats (nth beats-by-period period)
                             [r1 r2] (sort pair)]
                         (when (< beats max-beat-freq)
                           {:period period
                            :root-hz (exact.utils/->native root)
                            :root (cps->name* root)
                            :degree-1 deg1
                            :degree-2 deg2
                            :ratio-1 r1
                            :ratio-2 r2
                            :ratio-1-partial pr1
                            :ratio-2-partial pr2
                            :beat-freq.ratio beats
                            :beat-freq.hz (float (exact.utils/->native beats))
                            :beat-freq.factors (factorize-ratio beats)}))))))
        ratio-pairs))
     (range)
     root-periods-freqs)))

(comment
  ;; degrees 9&14 harmonics 6 & 5 should beat at 1hz
  (->> (get-beat-data (exact.utils/->exact 2)
                      (exact.utils/->exact 1)
                      (map exact.utils/->exact (range 1 7))
                      [67/64
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
                       125/64
                       2/1])
       (filter (fn [{:keys [period degree-1 degree-2]}]
                 (let [degs #{9 14}]
                   (and
                    (= 1 period)
                    (degs degree-1)
                    (degs degree-2)))))
       #_#_(map :beat-freq.hz)
         sort)
  (exact.utils/make-readable (take 10 (get-beat-data (exact.utils/->exact 2)
                                                     (exact.utils/->exact 1)
                                                     (map exact.utils/->exact (range 1 6))
                                                     ratios)))
  (count (exact.utils/make-readable (get-beat-data (exact.utils/->exact 2)
                                                   (exact.utils/->exact 1)
                                                   (map exact.utils/->exact (range 1 6))
                                                   (parse-ratios "1 5/4 3/2")))))
