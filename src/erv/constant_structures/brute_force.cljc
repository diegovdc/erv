(ns erv.constant-structures.brute-force
  "Find constant structures by brute force"
  (:require
   [clojure.math.combinatorics :refer [combinations]]
   [erv.constant-structures.core :refer [analyze maybe-round]]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [interval]]
   [erv.utils.ratios :refer [ratios->scale]]
   [taoensso.timbre :as timbre]))

(def scale
  [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
   {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
   {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
   {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
   {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
   {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
   {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
   {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}])

(defn make-test-ratios
  [period period-cents cents-step added-data-fn]
  (->> (range cents-step (+ period-cents cents-step) cents-step)
       (remove (fn [cents] (> cents period-cents)))
       (map conv/cents->ratio)
       (ratios->scale period)
       (map #(assoc % :added-note? true))
       (map added-data-fn)))

;; TODO use transducer
(defn find-1
  [scale period cent-step total-added-notes added-notes-data-fn]
  (let [added-notes* (make-test-ratios period
                                       (conv/ratio->cents period)
                                       cent-step
                                       added-notes-data-fn)
        added-notes (combinations added-notes* total-added-notes)
        _ (println "Total combinations to test: " (count added-notes))
        test-scales (map (fn [notes] (->> (apply conj scale notes)
                                          (sort-by :bounded-ratio)))
                         added-notes)

        _ (println "Total scales to test: " (count test-scales))
        constant-structures  (->> test-scales
                                  (filter (fn [scale]
                                            (->> (analyze scale)
                                                 :constant-structure?)))
                                  (map (fn [scale]
                                         (let [scale-i (map-indexed (fn [i n] (assoc n :degree i))  scale)]
                                           {:scale scale-i
                                            :added-notes (filter :added-note? scale-i)})))
                                  (group-by (comp (partial map :degree) :added-notes))
                                  (map (fn [[degs scales]] (-> scales
                                                               first
                                                               (assoc :degrees degs)))))]

    (println "Testing done!")
    {:total (count constant-structures)
     :constant-structures constant-structures}))

(comment
  (find-1 scale 3 100 4 (fn [n] (assoc n  :color [255 0 0])))
  (require '[erv.constant-structures.graphics :refer [init-cs-tool! update-state]])
  (def brute-force-results (find-1 scale 3 100 4 (fn [n] (assoc n  :color [255 0 0]))))
  (def s (init-cs-tool! scale    interval-steps (updated-data interval)
                        []))
  (-> brute-force-results
      :constant-structures
      (nth 0)
      :added-notes
      (->> (map :bounded-ratio)))
  (update-state s
                scale
                (-> brute-force-results
                    :constant-structures
                    (nth 0)
                    :added-notes
                    (->> (map :bounded-ratio)))))
(map-indexed vector (range 10 20))

(defn ^:deprecated cs-subsets
  "A naive implementation for checking `cs-subsets`. It's less performant than `quick-cs-subsets`. So prefer that."
  [min-scale-size scale]
  (->>  (range min-scale-size (count scale))
        (mapcat (fn [i] (combinations scale i)))
        (filter (fn [scale]
                  (:constant-structure? (analyze scale))))))

(defn- get-interval
  "Get interval from a sorted pair of degrees"
  [scale deg-pair]
  (let [[deg1 deg2] (sort deg-pair)
        a (:bounded-ratio (nth scale deg1))
        b (:bounded-ratio (nth scale deg2))]
    (maybe-round (interval a b))))

(def ^:private memoized-deg-combinations
  (memoize (comp #(map sort %)
                 (fn [scale-size]
                   (combinations (range scale-size)
                                 2)))))

(def ^:private conj-set (fnil conj #{}))

(defn- quick-check-cs?
  [deg-combinations scale]
  (reduce
   (fn [data deg-pair]
     (let [[deg1 deg2] deg-pair
           steps (- deg2 deg1)
           interval (get-interval scale deg-pair)
           updated-data (update data interval conj-set steps)
           interval-steps (updated-data interval)]
       (if (> (count interval-steps) 1)
         (reduced {})
         updated-data)))
   {}
   deg-combinations))

(defn quick-cs-subsets
  [cs-sizes scale]
  (eduction
   (mapcat #(combinations (range (count scale)) %))
    ;;  TODO may be possible to optimize with `pmap`
   (keep (fn [degs-combination]
           (let [deg-pairs (memoized-deg-combinations (count degs-combination))
                 subscale (map #(nth scale %) degs-combination)]
             (when (seq (quick-check-cs? deg-pairs subscale))
               (map #(nth scale %) degs-combination)))))
   cs-sizes))

(defn take-quick-cs-subsets
  "Takes n-items after dropping n-items. Takes an `eduction` as returned by `quick-cs-subsets`.
  NOTE this code is a general solution, but has specific names for documentation purposes.
  The eduction does not cache results, but doesn't chunk them either as `sequence` does.
  If caching is needed on can do: `(sequence (quick-cs-subsets))`"
  [n-dropped n-taken quick-cs-subsets-lazy-seq]
  (into []
        (comp (drop n-dropped) (take n-taken))
        quick-cs-subsets-lazy-seq))

(comment
  (/ (count-combinations (range 41) 8) 290.0)
  (take 10 (sequence (mapcat (fn [i] (range (inc i)))) (range)))

  (defn find-odds []
    (eduction
     (mapcat #(combinations (range 41) %))
     (filter (fn [combination]
               (every? odd? combination)))
     (map (fn [x] (swap! found inc) x))
     [7]))

  (def found  (atom 0))
  (def my-odds (sequence (find-odds)))
  (take-quick-cs-subsets 2 3 my-odds)
  (into [] (comp (drop 1) (take 1)) (find-odds))
  (-> @found)

  () (take 1) (find-odds)

  (def found  (atom 0))
  (defn find-odds []
    (sequence
     (comp
      (mapcat #(combinations (range 40) %))
      (keep (fn [combination]
              (when (every? odd? combination)
                (swap! found inc)
                (println "found" combination)
                combination))))
     [7 8]))

  (into [] (take 1) (find-odds))
  (-> @found)

  (defn find-odds-eduction []
    (eduction
     (mapcat #(clojure.math.combinatorics/combinations (range 40) %))
     (keep (fn [combination]
             (when (every? odd? combination)
               (swap! found inc)
               (println "found" combination)
               combination)))
     [7 8]))

  (take 1
        (find-odds-eduction))

  (mapcat #(combinations (range 4) %) [2 3])
  (require '[clojure.math.combinatorics :refer [count-combinations]])
  (count-combinations (range 40) 7))
