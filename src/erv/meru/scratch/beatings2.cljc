(ns erv.meru.scratch.beatings2
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [erv.utils.conversions :refer [cps->name*]]
   [erv.utils.core :refer [decompose-ratio factorize-ratio factors->hiccup
                           make-map-by-key pow prime-factors]]
   [erv.utils.ratios :refer [ratios->scale-data]]
   [taoensso.timbre :as timbre]
   #?(:clj [hiccup2.core :as h])))

(comment
  (def a 1))
#_(def c (* 3 11 8))

(def c 256 #_(* 3 11 8))

(defn get-beat-data-by-pairs
  ([ratios] (get-beat-data-by-pairs (range 1 9) ratios))
  ([partials ratios]
   (->> ratios
        (mapcat
         (fn [degree ratio]
           (map (fn [i] {:degree degree
                         :ratio ratio
                         :partial i
                         :partial*ratio (* i ratio)})
                partials))
         (range))
        (#(combo/combinations % 2))
        (remove (fn [[x1 x2]] (= (:ratio x1) (:ratio x2))))
        (map (fn [pair] {:pair pair
                         :diff (abs (- (:partial*ratio (first pair))
                                       (:partial*ratio (second pair))))}))
        (sort-by :diff)
        #_(map (fn [pair]
                 (assoc pair
                        :diff-c4 (double (* root (:diff pair)))
                        :diff-c3 (double (/ (* root (:diff pair))
                                            2))
                        :diff-c2 (double (/ (* root (:diff pair))
                                            4))
                        :diff-c1 (double (/ (* root (:diff pair))
                                            8))))))))

(comment

  (def metameantone-beatings)
  (get-beat-data-by-pairs [1
                           67/64
                           279/256
                           9/8
                           75/64
                           39/32
                           5/4
                           167/128
                           87/64
                           45/32
                           187/128
                           3/2
                           25/16
                           417/256
                           27/16
                           7/4
                           233/128
                           15/8
                           125/64])
  (get-beat-data-by-pairs [1
                           5/4
                           3/2
                           7/4]))

(do
  (defn- get-root-note-lowest-freq
    [period freq]
    (let [lowest-freq 20]
      (loop
       [freq freq]
        (cond
          (< freq lowest-freq) (recur (* freq period))
          (>= freq (* lowest-freq period)) (recur (/ freq period))
          (and (>= freq lowest-freq) (> (* lowest-freq period) freq)) freq))))
  (get-root-note-lowest-freq 2 20)
  (get-root-note-lowest-freq 2 40)
  (get-root-note-lowest-freq 2 16))

(do
  (defn- get-freq-periods-range
    [period initial-freq max-freq]
    (->> (range)
         (map #(* initial-freq (pow period %)))
         (take-while #(<= % max-freq))))
  (get-freq-periods-range 2 1 4000))

;; TODO refactor to smaller functions and rename

(defn get-beat-data
  "Using a root note, map the beat-data over the a range from 20 to 4000hz"
  [period root partials ratios]
  (let [max-freq 8000
        beat-data (get-beat-data-by-pairs partials ratios)
        lowest-root (get-root-note-lowest-freq period root)
        root-periods-freqs (get-freq-periods-range period lowest-root max-freq)
        pair->beat-data (->> beat-data
                             (mapv
                              (fn [pair]
                                (->> root-periods-freqs
                                     (mapv
                                      (fn [period root-freq]
                                        (let [k (keyword (str "diff-period-" period))]
                                          #_(double) (* root-freq (:diff pair))))
                                      (range))
                                     #_(into {})
                                     (assoc pair
                                            :root-hz lowest-root
                                            :beat-hz-by-period))))
                             (group-by  (comp set #(map :ratio %) :pair))
                             (mapv (fn [[k v]]
                                     [k (sort-by (juxt
                                                  (comp :partial first :pair)
                                                  (comp :partial second :pair))
                                                 v)]))
                             (into {})
                             #_(make-map-by-key  (comp set #(map :ratio %) :pair)))
        ratio-pairs (->> (keys pair->beat-data)
                         (sort-by (juxt first second)))]
    (mapcat
     (fn [period root]
       (mapcat
        (fn [pair]
          (->> (pair->beat-data pair)
               (keep (fn [beat-data]
                       (let [data ((juxt (comp (juxt :partial :degree) first :pair)
                                         (comp (juxt :partial :degree) second :pair)
                                         :beat-hz-by-period)
                                   beat-data)
                             [[pr1 deg1] [pr2 deg2] beats-by-period] data
                             beats (nth beats-by-period period)
                             [r1 r2] (sort pair)]
                         (when (and (< beats 20)
                                    (not (zero? beats)))
                           {:period period
                            :root-hz root
                            :root (cps->name* root)
                            :degree-1 deg1
                            :degree-2 deg2
                            :ratio-1 r1
                            :ratio-2 r2
                            :ratio-1-partial pr1
                            :ratio-2-partial pr2
                            :beat-freq.ratio beats
                            :beat-freq.hz (float beats)
                            :beat-freq.factors (factorize-ratio beats)}))))))
        ratio-pairs))
     (range)
     root-periods-freqs)))

(defn +beat-data
  [root-freq partials scale-data]
  (let [beat-data (get-beat-data (-> scale-data :meta :period)
                                 root-freq
                                 partials
                                 (map :bounded-ratio (:scale scale-data)))
        root (-> beat-data first :root-hz)
        root-kw (keyword (str root "hz"))]
    (when (not= root-freq root)
      (timbre/info "root-freq normalized to:" root))
    #?(:clj
       (timbre/info (format "Access beat data like: (-> scale-data :beat-data %s)" root-kw)))
    (assoc-in scale-data
              [:beat-data root-kw]
              beat-data)))
(comment
  (+beat-data
   1
   (range 1 6)
   (ratios->scale-data [1
                        67/64
                        279/256
                        9/8
                        75/64
                        39/32
                        5/4
                        167/128
                        87/64
                        45/32
                        187/128
                        3/2
                        25/16
                        417/256
                        27/16
                        7/4
                        233/128
                        15/8
                        125/64]))
  (->> (get-beat-data 2 1 #_[1 5/4 3/2 7/4]
                      (range 1 6)
                      [1
                       67/64
                       279/256
                       9/8
                       75/64
                       39/32
                       5/4
                       167/128
                       87/64
                       45/32
                       187/128
                       3/2
                       25/16
                       417/256
                       27/16
                       7/4
                       233/128
                       15/8
                       125/64]
                      #_[4181/4096
                         2178309/2097152
                         17/16
                         17711/16384
                         9227465/8388608
                         9/8
                         75025/65536
                         39088169/33554432
                         305/256
                         317811/262144
                         165580141/134217728
                         323/256
                         1346269/1048576
                         21/16
                         5473/4096
                         5702887/4194304
                         89/64
                         1449/1024
                         24157817/16777216
                         377/256
                         98209/65536
                         102334155/67108864
                         1597/1024
                         104005/65536
                         13/8
                         6765/4096
                         1762289/1048576
                         55/32
                         28657/16384
                         933147/524288
                         233/128
                         121393/65536
                         31622993/16777216
                         987/512
                         514229/262144
                         2/1])
       reverse
       #_(map :beat-freq)
       #_(map #(update % :beat-freq float))
       #_#_#_frequencies
           (sort-by second)
         reverse)
  (def scale-data (+beat-data
                   1
                   (range 1 6)
                   (ratios->scale-data [1
                                        67/64
                                        279/256
                                        9/8
                                        75/64
                                        39/32
                                        5/4
                                        167/128
                                        87/64
                                        45/32
                                        187/128
                                        3/2
                                        25/16
                                        417/256
                                        27/16
                                        7/4
                                        233/128
                                        15/8
                                        125/64]))))
(defn beat-factors-hiccup
  [{:keys [numer denom]}]
  [:span  (factors->hiccup numer) "/" (factors->hiccup denom)])

(defn hiccup-table [beat-data]
  (let [data-by-deg-pairs (group-by (comp (juxt :degree-1 :degree-2 :period)) beat-data)
        beat-freqs (->> beat-data
                        (group-by #(select-keys % [:beat-freq.hz :beat-freq.factors]))
                        (map (fn [[m vs]] (assoc m :instances (count vs))))
                        (sort-by :beat-freq.hz))
        rows (->> data-by-deg-pairs
                  (sort-by (comp (juxt #(nth % 2 nil) first second) first))
                  (map
                   (fn [[deg-pair pair-beat-data]]
                     (println (:root-hz (first pair-beat-data)))
                     (let [data-by-hz (group-by :beat-freq.hz pair-beat-data)
                           beating-harmonics-at-beat-hz-index (map
                                                               (fn [{hz :beat-freq.hz}]
                                                                 [:td
                                                                  {:style {:white-space "nowrap"}}
                                                                  (->> hz
                                                                       data-by-hz
                                                                       (map (comp #(str/join "," %) (juxt :ratio-1-partial :ratio-2-partial)))
                                                                       (str/join " "))])
                                                               beat-freqs)]
                       (into [:tr
                              [:td (nth deg-pair 2) "@" (:root-hz (first pair-beat-data)) "hz"]
                              [:td (str/join "," (take 2 deg-pair))]]
                             beating-harmonics-at-beat-hz-index)))))
        thead [:thead (into [:tr
                             [:th "Period"]
                             [:th "Degrees"]]
                            (map (fn [bf]
                                   [:th (:beat-freq.hz bf) "hz " "(" (:instances bf) ")"
                                    [:br]
                                    (beat-factors-hiccup  (:beat-freq.factors bf))])
                                 beat-freqs))]]
    [:table thead [:tbody rows]]
    #_beat-freqs
    #_data-by-deg-pairs)
  #_(h/html [:p]))

(comment
  (hiccup-table (:32hz (:beat-data scale-data))))

(defn html-template [body]
  [:html [:head [:style
                 "
table, th, td {
  border: 1px solid black;
  border-collapse: collapse;
}

thead th {
  position: sticky; /* Makes the header cells sticky */
  top: 0;
  background: #fff; /* Prevents content from showing through */
  z-index: 1;       /* Ensures the header stays above the scrolling rows */
}

th {
min-width: 70px;
padding: 0 4px;
}
"]] [:body body]])
#?(:clj
   (defn spit-html-table
     [out-path beat-data]
     (let [hiccup-data (-> beat-data
                           hiccup-table
                           html-template)]
       (->> hiccup-data
            h/html
            str
            (spit out-path)))))
(comment
  (spit-html-table "resources/meta-meantone-beats.html" (:32hz (:beat-data scale-data))))
