(ns erv.edo.voice-leading
  "Tools for finding chords that lead to other chords in an efficient way.
  Inspired by Dmitri Tymoczko's \"A Geometry of Music\" "
  (:require
   [clojure.core.async :as a]
   [clojure.set :as set]
   [erv.edo.chords :refer [chords-in-pattern]]
   [erv.utils.core :refer [get-all-rotations pattern->degrees]]
   [erv.utils.scale :refer [scale-steps->degrees]]))

(defn degrees->intervals
  [scale-size degrees]
  (let [intervals-minus-rest
        (->> degrees
             sort
             (partition 2 1)
             (mapv (fn [[a b]] (- b a))))
        rest* (- scale-size (apply + intervals-minus-rest))]
    (conj intervals-minus-rest rest*)))

(defn find-closest-chord
  ([edo chord] (find-closest-chord edo chord chord))
  ([edo chord target-chord]
   (let [intervals (degrees->intervals edo target-chord)
         rotations (map pattern->degrees (get-all-rotations intervals))
         transpositions (mapcat
                         #(map (fn [chord] (map (fn [n] (+ n %)) chord)) rotations)
                         (range (* -1 edo) edo))]
     (->> transpositions
          (map (fn [t]
                 (let [mvmnt (map - t chord)]
                   {:initial-chord chord
                    :target-chord t
                    :total-movement (apply + mvmnt)
                    :voice-movement mvmnt
                    :initial-intervals (degrees->intervals edo chord)
                    :target-intervals (degrees->intervals edo t)})))
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

(defn get-closest-chords
  [{:keys [scale-size possible-chords max-movement]} last-chord]
  (mapcat #(->> %
                (find-closest-chord scale-size last-chord)
                (take-while (fn [{:keys [total-movement]}]
                              (<= (abs total-movement) max-movement))))
          possible-chords))

(comment
  (def cc (get-closest-chords
           {:scale-size 12
            :possible-chords (map scale-steps->degrees [[4 3 5] [3 4 5]])
            :max-movement 4}
           [0 4 7])))

(defn filter-chords
  [scale-size scale-degrees* closest-chords*]
  (->> closest-chords*
       (filter (fn [{:keys [target-chord]}]
                 (set/subset?
                  (->> target-chord
                       (map #(mod % scale-size))
                       set)
                  scale-degrees*)))))

(comment
  (-> cc)
  (map :target-chord cc)
  (map :target-chord (filter-chords 12 (set (scale-steps->degrees [2 2 1 2 2 2 1])) cc)))

(defn get-chord-seqs
  [{:keys [scale-size _max-movement _possible-chords scale-degrees* degrees-by-chord] :as data}
   chord-seq]
  (let [last-chord (:chord (last chord-seq))
        closest-chords* (get-closest-chords data last-chord)
        next-chords (filter-chords scale-size scale-degrees* closest-chords*)
        chord-seqs  (map (fn [{:keys [target-chord total-movement]}]
                           (let [ch {:chord target-chord
                                     :mvt total-movement
                                     :degree (->> target-chord
                                                  (map #(mod % scale-size))
                                                  set
                                                  degrees-by-chord)}]
                             (conj chord-seq
                                   ch)))
                         next-chords)]
    {:next-chords next-chords
     :chord-seqs chord-seqs}))

(comment
  (get-chord-seqs
   {:scale-size 12
    :possible-chords (map scale-steps->degrees [[4 3 5] [3 4 5]])
    :max-movement 4
    :scale-degrees* (set (scale-steps->degrees [2 2 1 2 2 2 1]))
    :degrees-by-chord {#{2 9 5} 2, #{7 4 11} 4, #{0 4 9} 9, #{0 7 4} 0, #{0 9 5} 5, #{7 2 11} 7}}
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
(do
  (defn possible-chords
    [scale-steps chords-steps]
    (->> chords-steps (mapcat #(chords-in-pattern scale-steps %))))

  (->> (possible-chords
        [1 1 2 1 1 2 1 2]
        #{[5 4 2] [4 5 2] [4 4 3]})
       (sort-by :degree))
  #_[(chords-in-pattern
      [1 1 2 1 1 2 1 2]
      [5 4 2])
     (chords-in-pattern
      [1 1 2 1 1 2 1 2]
      [4 5 2])
     (chords-in-pattern
      [1 1 2 1 1 2 1 2]
      [4 4 3])])

(defn chord-progression
  [{:keys [scale-size
           scale-steps
           chords-steps
           max-len
           max-movement
           initial-chord-degs
           limit]
    :or   {limit 10000}}]
  (let [chans            (map (fn [_] (a/chan)) (range max-len))
        stop-chan        (a/chan)
        scale-degrees*   (set (scale-steps->degrees scale-steps))
        possible-chords* (possible-chords scale-steps chords-steps)
        degrees-by-chord (reduce (fn [acc {:keys [chromatic/degrees]}]
                                   (let [degree (first degrees)]
                                     (assoc acc (set degrees) degree)))
                                 {}
                                 possible-chords*)
        _                (println degrees-by-chord)
        possible-chords  (map #(scale-steps->degrees %) chords-steps)
        progressions     (atom [])
        pending-count    (atom 0)
        save-progression (fn [chord-seq] (swap! progressions conj chord-seq))
        make-loop        (fn [i chan]
                           (a/go-loop []
                             (let [chord-seq (a/<! chan)]
                               (if (nil? chord-seq)
                                 (when-let [chan (nth chans (inc i) nil)]
                                   #_(a/close! chan)
                                   (println "closing" (inc i)))
                                 (let [_                                (swap! pending-count dec)
                                       {:keys [next-chords chord-seqs]} (get-chord-seqs {:scale-size       scale-size
                                                                                         :max-movement     max-movement
                                                                                         :possible-chords  possible-chords
                                                                                         :scale-degrees*   scale-degrees*
                                                                                         :degrees-by-chord degrees-by-chord}
                                                                                        chord-seq)]
                                   (cond
                                     (> (count @progressions) limit) (println "stopping")
                                     (nil? (seq next-chords))        (save-progression chord-seq)

                                     (>= (count (first chord-seqs)) max-len) (doseq [chord-seq* chord-seqs]
                                                                               (save-progression chord-seq*))

                                     :else (doseq [chords chord-seqs]
                                             (let [repetition-type (chord-repeats? chords)]
                                               (case repetition-type
                                                 :bad-repeat nil ;; throw away the progression
                                                 :cycle      (save-progression chords)
                                                 :no-repeat  (do
                                                               (a/>! (nth chans (dec (count chords))) chords)
                                                               (swap! pending-count inc))))))
                                   (recur))))))]
    (doseq [[i chan] (map-indexed vector chans)]
      (make-loop i chan))
    (a/>!! (nth chans 0)
           [{:chord  initial-chord-degs
             :degree (->> initial-chord-degs
                          (map #(mod % scale-size))
                          set
                          degrees-by-chord)}])
    (a/close! (nth chans 0))
    (swap! pending-count inc)
    {:progressions  progressions
     :pending-count pending-count}))

(defn min-note-diff-filter
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
(min-note-diff-filter
 2
 '[[{:chord [0 4 7], :degree 0}
    {:chord (-1 4 7), :mvt -1, :degree 4}
    {:chord (0 4 9), :mvt 3, :degree 9}]
   [{:chord (2 5 9), :mvt 3, :degree 2}
    {:chord (-1 2 7), :mvt -6, :degree 7}
    {:chord (0 4 7), :mvt 3, :degree 0}]])
(->> ps
     :progressions
     deref
     (take 10))

(comment
  (->> cc
       (map :target-chord))
  (def ps (chord-progression {:scale-size 12
                              :scale-steps [2 2 1 2 2 2 1]
                              :chords-steps #{[4 3 5] [3 4 5]}
                              :max-len 8
                              :max-movement 7
                              :initial-chord-degs [0 4 7]}))

  (-> ps :progressions deref count)
  (->> user/progresion-1 :progressions deref
       (group-by (fn [prog]
                   (map :degree prog)))
       (filter (fn [[k v]]

                 (> (count v) 1)))
       (map (fn [[k vs]]
              [k (->> vs (map #(->> %
                                    (map :chord))))])))
  (-> ps :pending-count deref)

  (->> ps
       :progressions
       deref
       (sort-by (juxt count (fn [chs] (->> chs
                                           (map #(:mvt % 0))
                                           (apply +)
                                           abs))))
       (map #(map :chord %))
       set
       count
       #_(filter #(and ((set %) 7)
                       ((set %) 5)))))
(comment
  (require '[clojure.string :as str])
  (->> cc
       (map :target-chord))
  (def ps (chord-progression {:scale-size 11
                              :scale-steps [2 1 1 1 2 1 1 1 1]
                              :chords-steps #{[3,2,3,3]
                                              [3,2,4,2]
                                              [4,3,2,2]
                                              [3,4,2,2]}
                              :max-len 3
                              :max-movement 3
                              :initial-chord-degs [0 3 5 7]}))

  (-> ps :progressions deref count)
  (-> ps :pending-count deref)
  (do)
  (some println '([2 5 9] (2 4 9)))

  (->> ps
       :progressions
       deref
       shuffle
       #_(take 10)
       (map (fn [prog] (mapv (comp #(str/join "-" %) :chord) prog))))
  (->> ps
       :progressions
       deref
       (sort-by (juxt count (fn [chs] (->> chs
                                           (map #(:mvt % 0))
                                           (apply +)
                                           abs))))
       (map #(map (comp #_(fn [s] (str/join "-" s))
                   :chord)
                  %))
       #_(drop 20)
       shuffle
       (min-note-diff-filter 2)
       (take 20)
       #_(map #(str/join " " %))
       #_(str/join "\n")
       #_(spit "11t_ssLssLsL-chord-progressions_on_443-245-425-chords.txt")))

(comment
  (def a (a/chan))
  (a/>!! a 4)
  (a/close! a)

  (a/go-loop []
    (if-let [x (a/<! a)]
      (do (println x)
          (recur))
      (println "bye"))))

(comment
  (require '[erv.mos.submos :as submos])
  (->> (submos/make-all-submos [1 1 2 1 1 2 1 2] 4)
       (filter :true-submos?)
       (filter #(= 3 (count (:pattern %))))
       first
       :submos)

  (scale-steps->degrees [1 1 2 1 1 2 1 2])
  ;; => (0 1 2 4 5 6 8 9)
  ;; [4 5 2] -> 2 6 0 
  (->> (scale-steps->degrees [4 4 3])
       (map #(mod (+ 3 %) 11)))

  (scale-steps->degrees [3 4 4])
  [{:mos [4 4 3],
    :degree 0,
    :mos-degrees [0 3 6],
    :parent-degrees [0 4 8],
    :rotation {:mos [4 4 3], :degree 0, :at-zero? true}}
   {:mos [4 4 3],
    :degree 1,
    :mos-degrees [1 4 7],
    :parent-degrees [1 5 9],
    :rotation {:mos [4 4 3], :degree 1, :at-zero? false}}
   {:mos [4 5 2],
    :degree 2,
    :mos-degrees [2 5 0],
    :parent-degrees [2 6 0],
    :rotation {:mos [2 4 5], :degree 0, :at-zero? true}}
   {:mos [4 4 3],
    :degree 3,
    :mos-degrees [3 6 1],
    :parent-degrees [3 7 0],
    :rotation {:mos [3 4 4], :degree 1, :at-zero? false}}
   {:mos [4 4 3],
    :degree 4,
    :mos-degrees [4 7 2],
    :parent-degrees [4 8 1],
    :rotation {:mos [3 4 4], :degree 2, :at-zero? false}}
   {:mos [5 4 2],
    :degree 5,
    :mos-degrees [5 0 3],
    :parent-degrees [5 10 3],
    :rotation {:mos [4 2 5], :degree 0, :at-zero? true}}
   {:mos [4 4 3],
    :degree 6,
    :mos-degrees [6 1 4],
    :parent-degrees [6 10 3],
    :rotation {:mos [4 3 4], :degree 1, :at-zero? false}}
   {:mos [4 4 3],
    :degree 7,
    :mos-degrees [7 2 5],
    :parent-degrees [7 0 4],
    :rotation {:mos [4 3 4], :degree 2, :at-zero? false}}])
