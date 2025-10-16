(ns erv.utils.ratios
  ;; TODO: improve namespace definition
  #?@
   (:clj
    [(:require
      [clojure.core :as core]
      [clojure.edn :as edn]
      [clojure.string :as str]
      [com.gfredericks.exact :as e]
      [erv.utils.conversions :as conv]
      [erv.utils.core :refer [gcd-of-list interval period-reduce prime-factors
                              round2]]
      [erv.utils.exact :as exact.utils])]
    :cljs
    [(:refer-clojure :exclude [> < + -  * / -  numerator denominator integer?
                               mod rem quot even? odd? min])
     (:require
      [clojure.core :as core]
      [clojure.string :as str]
      [erv.utils.exact :as exact.utils]
      [com.gfredericks.exact :as e :refer [> < + -  * / - mod min numerator denominator]]
      [erv.utils.conversions :as conv]
      [erv.utils.core :refer [gcd-of-list interval period-reduce round2 prime-factors]]
      [erv.utils.impl :as impl :refer [format]])]))

(defn ratio-proximity-list
  "Make a list of `ratios` that approximate a `target-ratio` in a list of `target-ratios`"
  ([target-ratios ratios] (ratio-proximity-list target-ratios ratios 33))
  ([target-ratios ratios tolerance-cents]
   (->> target-ratios
        (map (fn [target]
               {target
                (reduce
                 (fn [acc ratio]
                   (let [diff (- (conv/ratio->cents target)
                                 (conv/ratio->cents ratio))]
                     (if (> (abs diff) tolerance-cents)
                       acc
                       (conj acc {:ratio ratio
                                  :diff (round2 3 diff)}))))
                 []
                 ratios)}))
        (map-indexed (fn [degree data]
                       [degree
                        (sort-by (comp abs :diff) (first (vals data)))]))
        (sort-by first))))

(defn ratio-proximity
  "Calculates the proximity between `ratios` and a list of `target-ratios`.
  Tries to find the closest ratio to a given target-ratio."
  [target-ratios ratios]
  (map (fn [target]
         {target
          (:best (reduce
                  (fn [{:keys [i best] :as acc} ratio]
                    (let [diff (Math/abs
                                (- (conv/ratio->cents target)
                                   (conv/ratio->cents ratio)))
                          current {:best {:degree (inc i)
                                          :ratio ratio
                                          :target-ratio target
                                          :diff diff}
                                   :i (inc i)}]
                      (cond
                        (nil? best) current
                        (< diff (:diff best)) current
                        :else
                        (assoc acc :i (inc i)))))
                  {:i -1 :best nil}
                  ratios))})
       target-ratios))

(defn ratios-sequence [denominator ending-numerator]
  (map #(/ % denominator) (range denominator ending-numerator)))

(defn float->ratio [num]
  (let [total-decimals (-> (str num)
                           (str/split ".")
                           second
                           count)
        tens (Math/pow 10 total-decimals)]
    (e// (e/native->integer (* num tens)) (e/native->integer tens))))

(defn ratio-string->ratio
  [ratio-string]
  (let [[numer denom] (-> ratio-string (str/split #"/"))]
    #?(:clj (/ (edn/read-string numer)
               (edn/read-string denom))
       :cljs (e// (e/string->integer numer) (e/string->integer denom)))))

;; TODO: simplify definition
#?(:clj (defn analyze-ratio
          [ratio]
          (let [numerator* (if (integer? ratio)
                             ;; in case it's big int,or something like 1N
                             (int ratio)
                             (numerator ratio))
                denominator*  (if (integer? ratio)
                                1
                                (denominator ratio))]
            {:numerator numerator*
             :denominator denominator*
             :numer-factors (prime-factors numerator*)
             :denom-factors (prime-factors denominator*)}))
   :cljs  (defn analyze-ratio
            [ratio]
            (let [numerator*  (if-not (e/ratio? ratio) ;when receiving something like 1/1 the above cond will not return a ratio type
                                ratio
                                (e/numerator ratio))
                  denominator* (if-not (e/ratio? ratio)
                                 e/ONE
                                 (e/denominator ratio))]

              {:numerator numerator*
               :denominator denominator*
               :numer-factors (prime-factors numerator*)
               :denom-factors (prime-factors denominator*)})))

(defn ratio->factor-string
  [ratio]
  (->> ratio
       analyze-ratio
       ((juxt (comp #(str/join "." (if (seq %) (map exact.utils/->native %) [1])) :numer-factors)
              (comp #(str/join "." (if (seq %) (map exact.utils/->native %) [1])) :denom-factors)))
       (apply format "%s/%s")))

(defn seq-interval-analysis
  [ratios]
  {:rooted-seq (let [intervals (mapv #(/ % (first ratios)) ratios)]
                 (mapv (juxt identity ratio->factor-string conv/ratio->cents) intervals))
   :pairs (mapv (fn [ratio-pair]
                  (let [interval (apply interval ratio-pair)]
                    [(vec ratio-pair) ((juxt identity ratio->factor-string conv/ratio->cents) interval)]))
                (partition 2 1 ratios))
   :ratio-factorization (mapv (juxt identity ratio->factor-string) ratios)})

(defn ratios->scale
  ([ratios] (ratios->scale 2 ratios))
  ([period ratios]
   (->> ratios
        (mapv (fn [r]
                (let [ratio (period-reduce period r)]
                  {:ratio ratio
                   :bounded-ratio ratio
                   :bounding-period (exact.utils/->exact period)})))
        (sort-by :bounded-ratio)
        ;; impl/+degree ;; TODO: should this be used here?
        )))

(defn ratios->scale-data
  ([ratios] (ratios->scale-data 2 ratios))
  ([period ratios]
   (let [scale (ratios->scale period ratios)]
     {:meta {:period period
             :size (count scale)}
      :scale scale})))

(defn ratios-intervals
  "Get the intervals between the ratios in the sequence.
  To get a wrapped version as in a scale use `scale-intervals`"
  [ratios]
  (->> ratios
       (partition 2 1)
       (map #(apply interval %))))

(defn interval-seq->ratio-stack
  [size interval-seq]
  (loop [ratios [(exact.utils/->exact 1)]
         index 0]
    (if (= size (count ratios))
      ratios
      (recur (conj ratios (* (last ratios)
                             (nth interval-seq (core/mod index (count interval-seq)))))
             (inc index)))))

(defn normalize-ratios
  "Will return a vector of ratios sorted and normalized so that the smallest one is 1/1."
  ([ratios] (normalize-ratios nil ratios))
  ([period ratios]
   (let [period (when period (exact.utils/->exact period))
         ratios (mapv exact.utils/->exact ratios)
         min* (apply min ratios)
         ratios* (->> ratios sort (map #(/ % min*)))]
     (if period
       (map #(period-reduce period %) ratios*)
       ratios*))))

(normalize-ratios [1 3 4])

(defn ratios->harmonic-series
  [ratios]
  (let [denominators (map (fn [r] (if (int? r) r (denominator r))) ratios)
        anti-denom (apply * denominators)
        harmonics (map #(* anti-denom %) ratios)
        gcd (gcd-of-list harmonics)]
    (map #(/ % gcd) harmonics)))

(defn gen-chain
  "Create a chain of ratios starting from 1"
  [length generator]
  (->> (range length)
       (map (fn [i] (apply core/* (repeat i generator))))))
