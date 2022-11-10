(ns erv.utils.ratios
  (:require
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [round2]]))

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
