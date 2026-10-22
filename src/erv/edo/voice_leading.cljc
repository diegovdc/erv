(ns erv.edo.voice-leading
  "Tools for finding chords that lead to other chords in an efficient way.
  Inspired by Dmitri Tymoczko's \"A Geometry of Music\" "
  (:require
   [clojure.core.async :as a]
   [clojure.set :as set]
   [erv.edo.chords :refer [chords-in-pattern]]
   [erv.mos.submos :as submos]
   [erv.utils.core :refer [get-all-rotations pattern->degrees]]
   [erv.utils.scale :refer [degrees->steps scale-steps->degrees]]
   [taoensso.timbre :as timbre]))

(defn find-closest-chord
  ([edo chord] (find-closest-chord edo chord chord))
  ([edo chord target-chord]
   (let [intervals (degrees->steps edo target-chord)
         rotations (mapv pattern->degrees (get-all-rotations intervals))
         transpositions (mapcat
                         #(mapv (fn [chord] (mapv (fn [n] (+ n %)) chord)) rotations)
                         (range (* -1 edo) edo))]
     (->> transpositions
          (mapv (fn [t]
                  (let [mvmnt (map - t chord)]
                    {:initial-chord chord
                     :target-chord t
                     :total-movement (apply + mvmnt)
                     :voice-movement mvmnt
                     :initial-intervals (degrees->steps edo chord)
                     :target-intervals (degrees->steps edo t)})))
          (remove #(= (:chord %) chord))
          (sort-by (comp abs :total-movement))))))

(comment
  ;; TODO convert to tests
  (->> (find-closest-chord 14 [1 4 8 12])
       (take 10)
       (map (juxt :target-chord
                  :total-movement
                  :voice-movement
                  (juxt
                   :initial-intervals
                   :target-intervals))))
  (->> (find-closest-chord 12 '(0 4 7)
                           [0 4 7])
       (take 10)
       #_(map (juxt :target-chord :total-movement :voice-movement))))

(defn- get-closest-chords
  [{:keys [scale-size chord-shape-degs max-movement]} last-chord]
  (mapcat #(->> %
                (find-closest-chord scale-size last-chord)
                (take-while (fn [{:keys [total-movement]}]
                              (<= (abs total-movement) max-movement))))
          chord-shape-degs))

(comment
  (def cc (get-closest-chords
           {:scale-size 12
            :chord-shape-degs (map scale-steps->degrees [[4 3 5] [3 4 5]])
            :max-movement 4}
           [0 4 7])))

(defn- filter-chords
  [scale-size scale-degrees* closest-chords*]
  (->> closest-chords*
       (filter (fn [{:keys [target-chord]}]
                 (set/subset?
                  (->> target-chord
                       (map #(mod % scale-size))
                       set)
                  scale-degrees*)))))

(defn- get-chord-seqs
  [{:keys [scale-size
           _max-movement
           _chord-shape-degs
           scale-degrees
           chord->degree] :as data}
   chord-seq]
  (let [last-chord (:chord (last chord-seq))
        closest-chords* (get-closest-chords data last-chord)
        next-chords (filter-chords scale-size scale-degrees closest-chords*)
        chord-seqs  (mapv (fn [{:keys [target-chord total-movement]}]
                            (let [ch {:chord target-chord
                                      :mvt total-movement
                                      :degree (->> target-chord
                                                   (map #(mod % scale-size))
                                                   set
                                                   chord->degree)}]
                              (conj chord-seq ch)))
                          next-chords)]
    {:next-chords next-chords
     :chord-seqs chord-seqs}))

(comment
  (get-chord-seqs
   {:scale-size 12
    :chord-shape-degs (map scale-steps->degrees [[4 3 5] [3 4 5]])
    :max-movement 4
    :scale-degrees (set (scale-steps->degrees [2 2 1 2 2 2 1]))
    :chord->degree {#{2 9 5} 2, #{7 4 11} 4, #{0 4 9} 9, #{0 7 4} 0, #{0 9 5} 5, #{7 2 11} 7}}
   [{:chord [0 4 7] :degree 0}]))

(defn- chord-repeats?
  "Returns one of #{:cycle :bad-repeat :no-repeat}
  :cycle - first and last chord are the same
  :bad-repeat - a middle chord is repeated (internal loop)
  :no-repeat - no repetition"
  [chord-seq]
  (if (= (-> chord-seq first :chord)
         (-> chord-seq last :chord))
    :cycle
    (let [bad-repeat? (reduce (fn [acc {:keys [degree]}]
                                (let [acc* (update acc degree (fnil inc 0))]
                                  (if (> (get acc* degree) 1)
                                    (reduced true)
                                    acc*)))
                              {}
                              chord-seq)]
      (if (true? bad-repeat?)
        :bad-repeat
        :no-repeat))))

(comment
  (chord-repeats?
   '[{:chord [0 4 7], :degree 0}
     {:chord (-1 2 7), :mvt -3, :degree 7}
     {:chord (-1 2 7), :mvt 3, :degree 5}]))

(defn- chord-shape-degs
  [scale-steps chords-steps]
  (->> chords-steps (mapcat #(chords-in-pattern scale-steps %))))

(defn- default-chord->degree
  "Returns a mapping from chord-degree set to degree."
  [scale-steps chords-steps]
  (let [chord-shape-degs (chord-shape-degs scale-steps chords-steps)]
    (reduce (fn [acc {:keys [chromatic/degrees]}]
              (let [degree (first degrees)]
                (assoc acc (set degrees) degree)))
            {}
            chord-shape-degs)))

(defn submos-chord->degree
  "Takes a mos and a submos pattern and returns a mapping of chord-degree set to degree."
  [mos chord-submos]
  (->> (submos/make-submos-for-pattern mos chord-submos)
       (map (fn [{:keys [degree chromatic-degrees]}]
              [(set chromatic-degrees) degree]))
       (into {})))

(defn submos-chord-progression-config
  [initial-degree mos submos]
  (let [submos-data (submos/make-submos-for-pattern mos submos)
        initial-chord-degs (-> (nth submos-data initial-degree)
                               :chromatic-degrees)]
    {:scale-steps mos
     :chords-steps (->> submos-data (mapv :mos) set)
     :chord->degree (submos-chord->degree mos submos)
     :initial-chord-degs initial-chord-degs}))

(defn chord-progression
  [{:keys [scale-steps
           chords-steps
           chord->degree
           max-len
           max-movement
           initial-chord-degs
           limit]
    :or   {limit 10000}}]
  (let [chans            (map (fn [_] (a/chan)) (range max-len))
        ;; stop-chan        (a/chan)
        scale-size       (apply + scale-steps)
        scale-degrees*   (set (scale-steps->degrees scale-steps))
        chord->degree*   (or chord->degree
                             (default-chord->degree scale-steps chords-steps))
        ;; _                (println chord->degree*)
        chord-shape-degs (map #(scale-steps->degrees %) chords-steps)
        progressions     (atom [])
        progressions-p   #?(:clj (promise) :cljs progressions)
        deliver-progressions (fn [] #?(:clj (deliver progressions-p @progressions)
                                       :cljs nil))
        pending-count    (atom 0)
        save-progression (fn [chord-seq] (swap! progressions conj chord-seq))
        make-loop        (fn [i chan]
                           (a/go-loop []
                             (let [chord-seq (a/<! chan)]
                               (if (nil? chord-seq)
                                 (if-let [chan (nth chans (inc i) nil)]
                                   (do (a/close! chan)
                                       (timbre/debug "closing" (inc i)))
                                   (deliver-progressions))
                                 (let [_                                (swap! pending-count dec)
                                       {:keys [next-chords chord-seqs]} (get-chord-seqs
                                                                         {:scale-size       scale-size
                                                                          :max-movement     max-movement
                                                                          :chord-shape-degs chord-shape-degs
                                                                          :scale-degrees    scale-degrees*
                                                                          :chord->degree    chord->degree*}
                                                                         chord-seq)]
                                   (cond
                                     (> (count @progressions) limit) (do (deliver-progressions)
                                                                         (timbre/warn "Progressions limit exceeded"))
                                     (nil? (seq next-chords))        (save-progression chord-seq)

                                     ;; (>= (count (first chord-seqs)) max-len) (doseq [chord-seq* chord-seqs]
                                     ;;                                           (save-progression chord-seq*))

                                     :else (let [chords->repeat?  (reduce (fn [m chords]
                                                                            (assoc m chords (chord-repeats? chords)))
                                                                          {}
                                                                          chord-seqs)
                                                 repetition-types (set (vals chords->repeat?))]
                                             ;; keep the previous chord-seq as some branches may not continue now
                                             (when (:bad-repeat repetition-types)
                                               (save-progression chord-seq))
                                             (doseq [[chords repetition-type] chords->repeat?]
                                               (case repetition-type
                                                 :bad-repeat nil ;; throw away the progression
                                                 :cycle      (save-progression chords)
                                                 :no-repeat  (if (>= (count chords) max-len)
                                                               (save-progression chords)
                                                               (do (a/>! (nth chans (dec (count chords))) chords)
                                                                   (swap! pending-count inc)))))))
                                   (recur))))))]
    (doseq [[i chan] (map-indexed vector chans)]
      (make-loop i chan))
    (a/>!! (nth chans 0)
           [{:chord  initial-chord-degs
             :degree (->> initial-chord-degs
                          (map #(mod % scale-size))
                          set
                          chord->degree*)}])
    (a/close! (nth chans 0))
    (swap! pending-count inc)
    {:atom/progressions    progressions
     :promise/progressions progressions-p
     :pending-count        pending-count}))

(defn min-note-diff-filter
  "Filters a list of chord progressions and returns chord progressions where
  note changes between chords are >= `min-diff`"
  [min-diff chords]
  (->> chords
       (filter (fn [chord-seq]
                 (let [pairs (partition 2 1 chord-seq)]
                   (reduce (fn [bool [a b]]
                             (and bool
                                  (>= (- (count (:chord a)) (count (set/intersection (set (:chord a)) (set (:chord b)))))
                                      min-diff)))
                           true
                           pairs))))))

(comment ;; simple example
  (do
    (def ps (chord-progression {:scale-size 12
                                :scale-steps [2 2 1 2 2 2 1]
                                :chords-steps #{[4 3 5] [3 4 5]}
                                :max-len 2
                                :max-movement 7
                                :initial-chord-degs [0 4 7]}))
    @(:promise/progressions ps))

  (-> ps :pending-count deref)

  (->> ps
       :atom/progressions
       deref))

(comment ;; more complex example
  (require '[clojure.string :as str])
  (def ps (chord-progression (merge {:max-len 8
                                     :max-movement 3}
                                    (submos-chord-progression-config 0 [2 1 1 1 2 1 1 1 1] [2 2 2 3]))))

  (-> ps :atom/progressions deref count)
  (-> ps :pending-count deref)

  ;; printable progressions
  (->> ps :atom/progressions deref shuffle (take 10)
       (map (fn [prog] (mapv (comp #(str/join "-" %) :chord) prog))))
  ;; spit them to file
  (->> ps
       :atom/progressions
       deref
       (sort-by (juxt count (fn [chs] (->> chs
                                           (map #(:mvt % 0))
                                           (apply +)
                                           abs))))
       (min-note-diff-filter 1)
       (mapv #(mapv (comp #_(fn [s] (str/join "-" s))
                     :chord)
                    %))
       #_(drop 20)
       shuffle

       (take 20)
       #_(map #(str/join " " %))
       #_(str/join "\n")
       #_(spit "11t_ssLssLsL-chord-progressions_on_443-245-425-chords.txt")))


