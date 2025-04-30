(ns erv.mos.v3.core
  (:require
   [clojure.string :as str]
   [erv.utils.conversions :refer [ratio->cents]]
   [erv.utils.core :refer [coprime? interval period-reduce round2]]
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
(seq-intervals 2 [1 3/2 9/8])
(defn interval-frequencies
  [period ratios]
  (->> ratios
       (seq-intervals period)
       frequencies))
(interval-frequencies 2 [1 9/8 81/64 729/512 3/2 27/16 243/128])

(do
  (defn mos?
    [period ratios]
    (let [interval-freqs (interval-frequencies period ratios)
          interval-quantities (vals interval-freqs)]

      (and (= 2 (count (keys interval-freqs)))
           (apply coprime? interval-quantities))))

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
            :mos/sL-ratio.float (float (/ (ratio->cents L) (ratio->cents s)))
            :mos/sL-ratio.cents (ratio->cents (/ (ratio->cents L) (ratio->cents s)))
            :mos/generator gen
            :mos/normalized-by 1
            :mos/type :ratio}
     :scale (ratios->scale period ratios)}))
(do
  (defn gen->mos-ratios
    ([gen period] (gen->mos-ratios gen period 100))
    ([gen period max-len]
     (let [gen* (cond (int? gen) (bigint gen)
                      (rational? gen) gen
                      :else (rationalize gen))
           gen-seq (reductions * (repeat gen*))
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

  (map (comp (juxt :size :mos/pattern
                   :mos/pattern.name :mos/s.cents :mos/L.cents :mos/sL-ratio.float)
             :meta)
       (gen->mos-ratios  2 3))

  (map (comp #(select-keys %
                           [:size :mos/pattern
                            :mos/pattern.name :mos/s.cents :mos/L.cents :mos/sL-ratio.float])
             :meta)
       #_(comp (partial map :bounded-ratio) :scale)
       (gen->mos-ratios 11/8
                        (rationalize (round2 4 (erv.utils.conversions/cents->ratio 400)))
                        50))
  #_(map count (gen->mos 3/2 2 12)))

(comment
  (->> (gen->mos-ratios 7/4 3)
       (filter #(-> % :meta :size (= 53)))
       first
       :scale)
  (rotate-scale (:scale (nth (gen->mos-ratios  3/2 2) 2))
                2))
