(ns erv.utils.scale
  (:require
   [erv.utils.core :refer [interval rotate]]
   [erv.utils.ratios :refer [interval-seq->ratio-stack normalize-ratios
                             ratios->scale ratios-intervals]]))

(defn degree-stack
  "Generate a stack ratios from a single (degree) generator"
  [{:keys [scale gen offset]}]
  (sort-by :gen/index
           (loop
            [subset #{}
             ratio-subset #{}
             offset offset
             gen-index 0]
             (let [i (mod offset (count scale))
                   new-note (assoc (nth scale i)
                                   :gen/index gen-index)
                   ratio (:bounded-ratio new-note)]
               (if (ratio-subset ratio)
                 subset
                 (recur (conj subset new-note)
                        (conj ratio-subset ratio)
                        (+ offset gen)
                        (inc gen-index)))))))

(defn scale-intervals
  "Get the intervals of a scale"
  [scale]
  (let [sorted-scale (->> scale
                          (sort-by :bounded-ratio))
        last-interval [(last sorted-scale)
                       (update (first sorted-scale)
                               :bounded-ratio #(* % (:bounding-period (first sorted-scale))))]]
    (->> sorted-scale
         (partition 2 1)
         (into [])
         (#(conj % last-interval))
         (map (fn [[a b]] (interval (:bounded-ratio a)
                                    (:bounded-ratio b)))))))

(defn tritriadic
  "Make a scale from stacking a triad three times.
  https://en.xen.wiki/w/Tritriadic_scale"
  ([triad-ratios] (tritriadic 2 triad-ratios))
  ([period triad-ratios]
   (let [triad-ratios (normalize-ratios period triad-ratios)]
     {:meta {:scale :tritriadic
             :triad-ratios triad-ratios}
      :scale  (ratios->scale period
                             (map #(* (last triad-ratios) %)
                                  (interval-seq->ratio-stack
                                   (ratios-intervals triad-ratios) 7)))})))

(defn scale->stacked-subscale
  "Make a scale from a stack of generator steps from a parent scale.
  Size defines the maximum number of notes, but these may be less due to duplications."
  [{:keys [scale gen offset size period]
    :or {size (count scale)
         period 2}}]
  (let [scale (-> {:scale scale
                   :gen gen
                   :offset offset}
                  degree-stack
                  scale-intervals
                  (interval-seq->ratio-stack size)
                  (->> (ratios->scale period))
                  distinct)]
    {:meta {:scale :stacked-subscale
            :intervals (scale-intervals scale)
            :parent-scale scale
            :gen gen
            :starting-offset offset}
     :scale scale}))

(defn dedupe-scale
  "Remove duplicate notes from a scale. Uses `:bounded-ratio`"
  [scale]
  (->> scale
       (reduce (fn [{:keys [scale ratios] :as acc} note]
                 (if (ratios (:bounded-ratio note))
                   acc
                   {:scale (conj scale note)
                    :ratios (conj ratios (:bounded-ratio note))}))
               {:scale []
                :ratios #{}})
       :scale))

(defn rotate-scale
  [scale step]
  (let [period (-> scale first :bounding-period)
        ratios (map :bounded-ratio (rotate scale step))
        first-ratio (first ratios)]
    (ratios->scale period
                   (->> ratios (map #(/ % first-ratio))))))
