(ns erv.mos.v3.core
  #?(:cljs (:refer-clojure :exclude [+ -  * / -  numerator denominator integer?
                                     mod rem quot even? odd? rationalize]))
  (:require
   #?(:cljs [com.gfredericks.exact :as e :refer [* - - /]])
   [erv.utils.exact :as exact.utils #?@(:cljs [:refer [rationalize]])]
   [clojure.core :as core]
   [clojure.string :as str]
   [erv.utils.conversions :refer [ratio->cents]]
   [erv.utils.core :refer [coprime? interval period-reduce]]
   [erv.utils.ratios :refer [ratios->scale]]))

;; rational mos

;; TODO move to ratios
(defn seq-intervals
  [period ratios]
  (->> ratios
       sort
       (into [])
       (#(conj % period))
       (partition 2 1)
       (map #(apply interval %))))

(comment
  (seq-intervals (e/native->integer 2) (erv.utils.exact/parse-ratios "1 3/2 9/8")))

(defn interval-frequencies
  [period ratios]
  (->> ratios
       (seq-intervals period)
       frequencies))
#_(interval-frequencies 2 [1 9/8 81/64 729/512 3/2 27/16 243/128])

(defn mos?
  [period ratios]
  (let [interval-freqs (interval-frequencies period ratios)
        interval-quantities (vals interval-freqs)]
    (and (= 2 (count (keys interval-freqs)))
         (apply coprime? interval-quantities))))
(comment
  (mos? (erv.utils.exact/->exact 2) (erv.utils.exact/parse-ratios "1 3/2 9/8 27/16 81/64 243/128"))
  (mos? 2 [1 3/2 9/8 27/16 81/64 243/128]))

(defn ratios->mos-data
  [{:keys [period ratios gen]}] ()
  (let [ratio-intervals (seq-intervals period ratios)
        interval-freqs (interval-frequencies period ratios)
        [s L] (->> ratio-intervals set sort)]
    {:meta {:scale :mos
            :period period
            :size (count ratios)
            :intervals/ratios ratio-intervals
            :intervals/cents (map ratio->cents ratio-intervals)
            :mos/pattern.name  (str (interval-freqs s) "s" (interval-freqs L) "L")
            :mos/pattern (str/join (map (fn [interval] (if (= interval s) "s" "L")) ratio-intervals))
            :mos/s s
            :mos/s.cents (ratio->cents s)
            :mos/L L
            :mos/L.cents (ratio->cents L)
            :mos/sL-ratio (/ L s)
            :mos/sL-ratio.float (float (core// (ratio->cents L) (ratio->cents s)))
            :mos/sL-ratio.cents (ratio->cents (core// (ratio->cents L) (ratio->cents s)))
            :mos/generator gen
            :mos/normalized-by 1
            :mos/type :ratio}
     :scale (ratios->scale period ratios)}))

(defn gen->mos-ratios
  ([gen period] (gen->mos-ratios gen period 100))
  ([gen period max-len]
   (let [gen* (cond (int? gen) (#?(:clj bigint :cljs e/native->integer) gen)
                    (#?(:clj rational? :cljs e/ratio?) gen) gen
                    :else (rationalize gen))
         gen-seq (reductions * (repeat gen*))
         period (exact.utils/->exact period)
         mos-ratios (->> (range max-len)
                         (map (fn [i]
                                (->> (conj (take i gen-seq) period)
                                     (map #(period-reduce period %))
                                     sort)))
                         (filter #(mos? period %)))]

     (map (fn [ratios]
            (ratios->mos-data {:period period
                               :ratios ratios
                               :gen gen}))
          mos-ratios))))

(comment
  ;; TODO: nice scale, save somewhere
  (->> (gen->mos-ratios 7/4 3)
       (filter #(-> % :meta :size (= 53)))
       first
       :scale))
