(ns erv.utils.ratios
  (:require [erv.utils.conversions :as conv]))

(defn ratio-proximity
  "Calculates the proximity between `ratios` and a list of `target-ratios`. I tries to find the closest ratio to a given target-ratio"
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
