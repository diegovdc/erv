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
        _ (println "CC" (count closest-chords*))
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
    chord-seqs))

(get-chord-seqs
 {:scale-size 12
  :possible-chords (map scale-steps->degrees [[4 3 5] [3 4 5]])
  :max-movement 4
  :scale-degrees* (set (scale-steps->degrees [2 2 1 2 2 2 1]))
  :degrees-by-chord {#{2 9 5} 2, #{7 4 11} 4, #{0 4 9} 9, #{0 7 4} 0, #{0 9 5} 5, #{7 2 11} 7}}
 [{:chord [0 4 7] :degree 0}])

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
(defn chord-progression
  [{:keys [scale-size
           scale-steps
           chords-steps
           max-len
           max-movement
           initial-chord-degs
           limit]
    :or {limit 10000}}]
  (let [chans           (map (fn [_] (a/chan)) (range max-len))
        stop-chan       (a/chan)
        scale-degrees*  (set (scale-steps->degrees scale-steps))
        possible-chords* (->> chords-steps (mapcat #(chords-in-pattern scale-steps %)))
        degrees-by-chord (reduce (fn [acc {:keys [degree degrees]}]
                                   (println degrees)
                                   (assoc acc (set degrees) degree))
                                 {}
                                 possible-chords*)
        _ (println degrees-by-chord)
        possible-chords (map #(scale-steps->degrees %) chords-steps)
        progressions    (atom [])
        pending-count (atom 0)
        save-progression (fn [chord-seq] (swap! progressions conj chord-seq))
        make-loop (fn [i chan]
                    (a/go-loop []
                      (let [chord-seq  (a/<! chan)
                            _ (swap! pending-count dec)
                            last-chord (:chord (last chord-seq))
                            closest-chords* (mapcat #(->> %
                                                          (find-closest-chord scale-size last-chord)
                                                          (take-while (fn [{:keys [total-movement]}]
                                                                        (<= (abs total-movement) max-movement))))
                                                    possible-chords)
                            ;; _ (println "CC" (count closest-chords*))
                            ;; _ (def cc closest-chords*)
                            next-chords (->> closest-chords*
                                             (filter (fn [{:keys [target-chord]}]
                                                       (set/subset?
                                                        (->> target-chord
                                                             (map #(mod % scale-size))
                                                             set)
                                                        scale-degrees*))))
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
                        (cond
                          (> (count @progressions) limit) (println "stopping")
                          (nil? (seq next-chords)) (save-progression chord-seq)

                          (>= (count (first chord-seqs)) max-len) (doseq [chord-seq* chord-seqs]
                                                                    (save-progression chord-seq*))

                          :else (doseq [chords chord-seqs]
                                  (let [repetition-type (chord-repeats? chords)]
                                    (case repetition-type
                                      :bad-repeat nil ;; throw away the progression
                                      :cycle (save-progression chords)
                                      :no-repeat (do
                                                   (a/>! (nth chans (dec (count chords))) chords)
                                                   (swap! pending-count inc)))))))
                      (recur)))]
    (doseq [[i chan] (map-indexed vector chans)]
      (make-loop i chan))
    (a/>!! (nth chans 0) [{:chord initial-chord-degs
                           :degree (->> initial-chord-degs
                                        (map #(mod % scale-size))
                                        set
                                        degrees-by-chord)}])
    (swap! pending-count inc)
    {:progressions progressions
     :pending-count pending-count}))
(chord-repeats?
 '[{:chord [0 4 7], :degree 0}
   {:chord (-1 2 7), :mvt -3, :degree 7}
   {:chord (-1 2 7), :mvt 3, :degree 5}])

(comment
  (->> cc
       (map :target-chord))
  (def ps (chord-progression {:scale-size 12
                              :scale-steps [2 2 1 2 2 2 1]
                              :chords-steps #{[4 3 5] [3 4 5]}
                              :max-len 6
                              :max-movement 7
                              :initial-chord-degs [0 4 7]}))

  (-> ps :progressions deref count)
  (-> ps :pending-count deref)

  (->> ps
       :progressions
       deref
       (sort-by (juxt count (fn [chs] (->> chs
                                           (map #(:mvt % 0))
                                           (apply +)
                                           abs))))
       (map #(map :chord %))
       #_(filter #(and ((set %) 7)
                       ((set %) 5)))))
(comment
  (def |a (a/chan))
  (def |b (a/chan))

  (a/go-loop []
    (let [[result |chan] (a/alts! [|a |b])]
      (println [result (= |chan |a)])
      (recur)))

  (a/>!! |a 2)
  (a/>!! |b 3))
