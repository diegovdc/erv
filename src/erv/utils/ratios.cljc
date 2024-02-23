(ns erv.utils.ratios
  #?@
   (:clj
    [(:require
      [clojure.edn :as edn]
      [clojure.string :as str]
      [com.gfredericks.exact :as e]
      [erv.utils.conversions :as conv]
      [erv.utils.core :refer [interval period-reduce prime-factors round2]])]
    :cljs
    [(:require
      [clojure.string :as str]
      [com.gfredericks.exact :as e]
      [erv.utils.conversions :as conv]
      [erv.utils.core :refer [interval period-reduce round2 prime-factors]])]))

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
  (let [[numer denom] (-> ratio-string
                          (str/split #"/"))]
    #?(:clj (/ (edn/read-string numer)
               (edn/read-string denom))
       :cljs (e// (e/string->integer numer) (e/string->integer denom)))))

(do
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
              (let [ratio* (cond
                             #?(:clj (ratio? ratio) :cljs false) ratio
                             (float? ratio) (float->ratio ratio)
                             (and (string? ratio) (str/includes? ratio "/")) (ratio-string->ratio ratio)
                             (and (string? ratio) (seq ratio)) (e/string->integer ratio)
                             :else (throw (ex-info (str "Don't know how to convert ratio, received " ratio) {:ratio ratio})))
                    numerator*  (if-not (e/ratio? ratio*) ;when receiving something like 1/1 the above cond will not return a ratio type
                                  (e/integer->native ratio*)
                                  (e/integer->native (e/numerator ratio*)))
                    denominator* (if-not (e/ratio? ratio*)
                                   1
                                   (e/integer->native (e/denominator ratio*)))]
                {:numerator numerator*
                 :denominator denominator*
                 :numer-factors (prime-factors numerator*)
                 :denom-factors (prime-factors denominator*)}))))

(defn ratio->factor-string
  [ratio]
  (->> ratio
       analyze-ratio
       ((juxt (comp #(str/join "." (if (seq %) % [1])) :numer-factors)
              (comp #(str/join "." (if (seq %) % [1])) :denom-factors)))
       #?(:clj (apply format "%s/%s")
          :cljs ((fn [n d] (str n "/" d))))))

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
        (map (fn [r]
               (let [ratio (period-reduce period r)]
                 {:ratio ratio
                  :bounded-ratio ratio
                  :bounding-period period})))
        (sort-by :bounded-ratio))))
