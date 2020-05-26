(ns erv.scale.core
  (:require
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [validate wrap-at round2]]
   [erv.utils.sequencer :refer [play!]]
   [clojure.spec.alpha :as s]))

(s/def ::bounded-ratio ratio?)
(s/def ::bounding-period number?)
(s/def ::degree
  (s/keys :req-un [::bounded-ratio ::bounding-period]))
(s/def ::scale (s/and not-empty (s/* ::degree)))


(defn- get-transp-fn [scale-period]
  (if (>= scale-period 0) * /))

(defn- transpose-by [bounding-period scale-period]
  (apply (get-transp-fn scale-period)
         1 ;; important when transp-fn is division
         (repeat (Math/abs scale-period) bounding-period)))

(defn deg->freq
  "Given a `scale` as spec'd above, a `base-freq` and a `degree`,
  get a frequency corresponding to the degree.
  The `base-freq` is the frequency that which is closest to degrees 0 and -1.
  Degrees are zero-based.
  This function supports non-octave repeating scales. We call the interval of
  repetition the `period` of the scale. That is why for the `scale` each of its
  degrees must have `:bounding-period` and `:bounded-ratio` keys.
  If the degree is outside the range of the scale i.e. 7 in a 7 note scale, then
  the degree's frequency will be transposed to the corresponding `period`.
  For convenience the `:period` key can be used to transpose the resulting
  frequency, i.e 1 will play the degree one `period` above."
  [scale base-freq degree
   & {:keys [period] :or {period 0}}]
  {:pre [(validate ::scale scale)]}
  (let [scale-len (count scale)
        period* (+ period (quot degree scale-len))
        degree* (mod degree scale-len)
        {:keys [bounded-ratio bounding-period]} (nth scale degree*)
        period-transp (transpose-by bounding-period period*)]
    (* period-transp bounded-ratio base-freq)))

(defn intervals->degs [base-deg intervals]
  (reduce #(conj %1 (+ (last %1) %2))
          [base-deg]
          intervals))

(defn stateful-interval->degree
  "Returns a statennful function that takes an interval and returns a degree based
  on the previous calculated degree.
  NOTE `base-degree` and `interval` must be integers. If they are not,
  then they both will be cast to integers."
  ([] (stateful-interval->degree 0))
  ([base-degree]
   (let [prev-degree (atom (int base-degree))]
     (fn [interval]
       (swap! prev-degree + (int interval))))))

(defn demo-scale*
  "Creates a list of frequencies that run up and down a scale by the specified
  number of periods"
  ([scale] (demo-scale* scale 1 440 :up-down))
  ([scale periods] (demo-scale* scale periods 440 :up-down))
  ([scale periods base-freq] (demo-scale* scale periods base-freq :up-down))
  ([scale periods base-freq direction]
   (let [degrees (-> scale
                     count
                     (* periods)
                     inc ;; TODO add option for including this note (?)
                     range)
         degrees* (case direction
                    :up degrees
                    :down (reverse degrees)
                    :down-up (concat (reverse degrees) (rest degrees))
                    #_:up-down (concat degrees (rest (reverse degrees))))]
     (map #(deg->freq scale base-freq %) degrees*))))

(defn demo!
  [scale &
   {:keys [periods base-freq note-dur direction on-event]
    :or {periods 1
         base-freq 440
         note-dur 300
         direction :up-down
         on-event (fn [i freq] (-> (wrap-at i scale)
                                  (dissoc :bounding-period :bounded-ratio)
                                  (assoc :note (cps->name* freq))
                                  println))}}]
  (play! (demo-scale* scale periods base-freq direction) note-dur
         :on-event on-event))



(comment
  (require '[erv.utils.conversions :refer [ratio->cents cps->midi midi->cps]]
           '[erv.cps.core :as cps]
           '[clojure.test :refer [testing is]])

  (do
    (->> [13 7 2 9]
         (cps/->set 3)
         cps/set->maps
         (cps/bound-ratio 3)
         (cps/maps->data :bounded-ratio)
         :scale
         (#(cps/filter-scale % #{7}))
         #_user/spy
         (#(demo! %
                  :periods 1
                  :base-freq 320
                  :note-dur 300
                  :direction :up)))))
