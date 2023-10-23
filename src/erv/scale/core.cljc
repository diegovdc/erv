(ns erv.scale.core
  #?@
  (:clj
   [(:require
     [clojure.spec.alpha :as s]
     [clojure.string :as str]
     [erv.cps.core :as cps]
     [erv.utils.conversions :refer [cps->name* ratio->cents]]
     [erv.utils.core :refer [interval wrap-at]]
     [erv.utils.sequencer :refer [play!]]
     [table.core :as t]
     [taoensso.timbre :as timbre])]
   :cljs
   [(:require
     [clojure.spec.alpha :as s]
     [clojure.string :as str]
     [erv.utils.conversions :refer [cps->name* ratio->cents]]
     [erv.utils.core :refer [interval wrap-at]]
     [erv.utils.sequencer :refer [play!]]
     [taoensso.timbre :as timbre])]))

(s/def ::bounded-ratio number?)         ;; Used to be `ratio?` but changed to `number?` to support `edos`
(s/def ::bounding-period number?)
(s/def ::degree (s/keys :req-un [::bounded-ratio ::bounding-period]))
(s/def ::scale (s/and not-empty (s/* ::degree)))

(defn- get-transp-fn [scale-period]
  (if (>= scale-period 0) * /))

(defn transpose-by [bounding-period scale-period]
  (apply (get-transp-fn scale-period)
         1 ;; important when transp-fn is division
         (repeat (Math/abs (double scale-period)) bounding-period)))

(defn get-period [transp-period scale-len degree]
  (let [transp-period* (if (> 0 degree) (- transp-period 1) transp-period)
        ;; prevent the zeroth degree to be counted as the start of lower octave
        ;; else our tranposition algorithm will overtranspose this degree
        degree* (if (> 0 degree) (inc degree) degree)]
    (+ transp-period* (quot degree* scale-len))))

(defn deg->freq
  "Given a `scale` as spec'd above, a `base-freq` and a `degree`,
  get a frequency corresponding to the degree.
  The `base-freq` is the frequency which is closest to degrees 0 and -1.
  Degrees are zero-based.
  This function supports non-octave repeating scales. We call the interval of
  repetition the `period` of the scale. That is why for the `scale` each of its
  degrees must have `:bounding-period` and `:bounded-ratio` keys.
  If the degree is outside the range of the scale i.e. 7 in a 7 note scale, then
  the degree's frequency will be transposed to the corresponding `period`.
  For convenience the `:period` key can be used to transpose the resulting
  frequency, i.e 1 will play the degree one `period` above."
  [scale base-freq degree
   & {:keys [period debug-fn] :or {period 0}}]
  #_{:pre [(validate ::scale scale)]}
  (let [scale-len (count scale)
        period* (get-period period scale-len degree)
        degree* (mod degree scale-len)
        note (nth scale degree*)
        {:keys [bounded-ratio bounding-period]} note
        period-transp (transpose-by bounding-period period*)]
    (when debug-fn (timbre/info (debug-fn note)))
    (* period-transp bounded-ratio base-freq)))

(defn intervals->degs [base-deg intervals]
  (reduce #(conj %1 (+ (last %1) %2))
          [base-deg]
          intervals))

(defn interval->ratio
  "Get the ratio between two degrees in a scale"
  [scale origin-deg target-deg]
  (let [scale-len (count scale)
        bounding-period (:bounding-period (first scale))
        origin (:bounded-ratio (wrap-at origin-deg scale))
        target (:bounded-ratio (wrap-at target-deg scale))
        period* (get-period 0 scale-len (- target-deg origin-deg))
        period-transp (transpose-by bounding-period period*)]
    (* period-transp (interval origin target))))

(comment
  (require '[erv.cps.core :as cps])
  (:scale (cps/make 2 [1 3 5 7]))
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 2 4)
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 2 10)
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 2 8)
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 2 14)
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 8 2)
  (interval->ratio (:scale (cps/make 2 [1 3 5 7])) 8 -4))

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

(defn- pitch-name->pitch-class [name*]
  (let [op (if (str/includes? name* "+") "+" "-")]
    (-> name* (str/split (re-pattern (str "\\" op)))
        (update 0 (comp first #(str/split % #"\d")))
        (->> (str/join op)))))

(comment (pitch-name->pitch-class "G#5+45")
         (pitch-name->pitch-class "G#5"))

(defn +names
  "Add the note names to a scale"
  [base-freq scale]
  (map-indexed
   (fn [idx note-data]
     (assoc note-data
            :pitch
            (let [freq (deg->freq scale base-freq idx)
                  name* (cps->name* freq)]
              {:name name*
               :class (pitch-name->pitch-class name*)
               :base-freq base-freq})))
   scale))

(defn demo-scale*
  "Creates a list of frequencies that run up and/or down a scale by the specified
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
         base-freq 220
         note-dur #?(:clj 0.5 :cljs 0.5)
         direction :up-down
         on-event (fn [i freq] (-> (wrap-at i scale)
                                   (dissoc :bounding-period :bounded-ratio)
                                   (assoc :note (cps->name* freq))
                                   println))}}]
  (play! (demo-scale* scale periods base-freq direction)
         note-dur
         :on-event on-event))

(comment
  (require '[erv.utils.conversions :refer [ratio->cents cps->midi midi->cps]]
           '[erv.cps.core :as cps]
           '[clojure.test :refer [testing is]]
           '[clojure.string :as str]
           '[erv.utils.sequencer :refer [play!]])

  (demo! (:scale (cps/make 2 [1 3 5 7])))
  (let [freqs (map #(cps->midi (deg->freq scale 10 %)) (range (count scale)))]
    (str "[" (->> (map #(- % (first freqs)) freqs)
                  (str/join ", "))
         "]")))

(do
  (defn print-scale-intervals!
    [scale
     & {:keys [unit ratio-type]
        :or {unit :cents ;; #{:cents :ratios}
             ratio-type :bounded-ratio ;; #{:bounded-ratio :ratio}
             }}]
    (let [conversor (case unit
                      :ratios identity
                      (comp int ratio->cents))
          data (let [ratios (->> scale (map ratio-type))]
                 (map (fn [a b] (concat [a] b))
                      (concat ["ratio"] ratios)
                      (concat [(map #(str "  " % "  ") ratios)]
                              (map (fn [r]
                                     (map (fn [r2]  (conversor (interval r r2))) ratios))
                                   ratios))))]
      #?(:clj (t/table data)
         :cljs (js/console.table data))
      data))
  )

(comment
  (require '[erv.cps.core :as cps])
  (print-scale-intervals! (:scale (cps/make 2 [1 3 5 7]))))
