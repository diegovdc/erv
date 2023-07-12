(ns erv.cps.cycles.v2
  (:require
   [clojure.core :refer [spit]]
   [clojure.core.async :as a]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [taoensso.timbre]
   [clojure.set :as set]))

(comment
  ;; You can use mapcat to quickly dispatch a seq
  (def c (a/chan 3 (comp (mapcat identity)
                         (filter odd?))))
  ;;=> #'user/c
  (a/>!! c (range 3))
  ;;=> true
  (a/go-loop [x (a/<! c)]
    (when x ;; abort when channel is closed (nil? (<! c))
      (prn x)
      (recur (a/<! c))))
  ;;=> #object[clojure.core.async.impl.channels.ManyToManyChannel...
  ;; When provisioning more items than the channel buffer allows,
  ;; mapcat will still put all items

  (a/>!! c (range 20))

  (a/close! c))

(def hexany
  (cps/make 2 [1 3 5 7]))

(def hexany-graph
  (-> hexany :graphs :simple))

(do
  (defn add-nodes-to-cycle [graph cycle]
    (if-not (seq cycle)
      (throw (ex-info "cycle can not be empty" {:cycle cycle}))
      (let [next-nodes (graph (last cycle))]
        (map #(conj cycle %) next-nodes))))

  (add-nodes-to-cycle hexany-graph [#{7 1} #{1 5} #{7 5}]))

(do
  (defn filter-partial-cycles
    [cycles]
    (remove (fn [cy]
              (->> cy
                   frequencies
                   (filter (fn [[_ freq]] (> freq 1)))
                   (some #(not= (first %) (first cy)))))
            cycles))
  (filter-partial-cycles
   [[#{7 1} #{1 5} #{7 5} #{7 1}]
    [#{7 1} #{1 5} #{7 5} #{3 5}]
    [#{7 1} #{1 5} #{7 5} #{7 3}]
    [#{7 1} #{1 5} #{7 5} #{1 5}]]))

(defn cycle-complete?
  [cycle]
  (= (first cycle) (last cycle)))

(defn readble-cycle [archi-factors cycle]
  (->> cycle
       (map (fn [set*] (str/join "." (map archi-factors (sort set*)))))
       (str/join " - ")))

(defn send-cycles-to-next-channel
  [chan-map writer-chan archi-factors cycle]
  (a/go (let [chan (rand-nth (get chan-map (count cycle)))]
          (cond
            (cycle-complete? cycle) (a/>! writer-chan (str (readble-cycle archi-factors cycle) "\n"))

            chan (a/>! chan cycle)
            :else (taoensso.timbre/error "No channel for this cycle" cycle)))))

(defn make-processor [{:keys [cps output-file max-cycle-len filter-fn]
                       :or {max-cycle-len 12
                            filter-fn identity}}]
  (let [graph (-> cps :graphs :simple)
        archi-factors (->> cps :meta :cps/factors (map-indexed (fn [i fac] {fac (str (char (+ 65 i)))})) (into {}))
        chans (map (fn [i] [(inc i) (map (fn [_] (a/chan 250))
                                         (range 4000))])
                   (range (inc max-cycle-len)))
        chan-map (into {} chans)
        writer-chan (a/chan 1000000)]
    (a/go-loop [line (a/<! writer-chan)]
      (when line
        #_(taoensso.timbre/info "Writing")
        (spit output-file line :append true)
        (recur (a/<! writer-chan))))
    (doseq [chan (flatten (vals chan-map))]
      (a/go-loop [cycle (a/<! chan)]
        (when cycle
          (let [next-cycles (->> cycle
                                 (add-nodes-to-cycle graph)
                                 filter-partial-cycles
                                 filter-fn)]
            #_(taoensso.timbre/info "More cycles")
            (doseq [cycle next-cycles]
              (a/<! (send-cycles-to-next-channel chan-map writer-chan archi-factors cycle))))
          (recur (a/<! chan)))))
    (assoc chan-map :writer writer-chan)))

(comment
  (def hexany (cps/make 3 [1 3 5 7 11 13]))
  (doseq [[_ chan] chan-map]
    (a/close! chan))
  (def file-name "eikosany-2.txt")
  (def chan-map (make-processor
                 {:cps hexany
                  :output-file file-name
                  :max-cycle-len 21
                  :filter-fn (fn [cycles]
                               (filter weak-harmonic-triad-cycle? cycles))}))
  (spit file-name "")
  (a/>!! (-> chan-map (get 1) first) [#{1 3 5}])

  ;; buffer count per channel
  (-> chan-map (dissoc :writer)
      (->> (sort-by first) vals flatten (map #(.count (.buf %)))
           (remove zero?)
           sort))
  (-> chan-map (dissoc :writer)
      (->> (map (fn [[i bufs]] [i (float (/ (apply + (map #(.count (.buf %))
                                                          bufs))
                                            (count bufs)))]))))
  (-> chan-map :writer
      (#(.count (.buf %))))
  ;; contents of a buffer
  (->> (get chan-map 12) (#(.buf (.buf %))) last))

(do (defn weak-harmonic-triad-cycle?
      [cycle]
      (every? (fn [[a _b c]]
                (= 1 (count (set/difference c a))))
              (partition 3 1 cycle)))
    (defn strong-harmonic-triad-cycle?
      [cycle]
      (let [cycle* (concat (drop-last 1 cycle)
                           (take 2 cycle))]
        (every? (fn [[a _b c]]
                  (= 1 (count (set/difference c a))))
                (partition 3 1 cycle*))))
    (strong-harmonic-triad-cycle? [#{"C" "B" "A"} #{"C" "F" "B"} #{"E" "C" "B"} #{"C" "B" "A"}]))

(comment
  (spit "test.txt" "")
  ;; writting to a file
  (spit  "test.txt"
         (str (random-uuid) "\n")
         :append true)
  (->> (slurp "eikosany-2.txt")
       (str/split-lines)
       (sort-by (juxt count))
       (str/join "\n")
       (spit "eikosany-harmonic-triad-cycles-D.E.F.txt"))
  (def cycles (slurp file-name))
  (set/difference #{"C" "B" "A"} #{"C" "F" "B"})

  (->> cycles
       (str/split-lines)
       sort
       (map #(->> (str/split % #" - ")
                  (map (fn [s] (set (str/split s #"\."))))))
       (filter strong-harmonic-triad-cycle?)
       (map #(->> %
                  (map (fn [s] (str/join "." (sort s))))
                  (str/join " - ")))
       (str/join "\n")
       (spit "eikosany-harmonic-triad-cycles-of-A.B.C.txt")))

(->> hexany
     :meta
     :cps/factors
     (map-indexed
      (fn
        [i fac]
        {fac (str (char (+ 65 i)))}))
     (into {}))
