(ns erv.scale.scl
  "The purpose of this namespace is to produce scala files."
  #?@
   (:clj
    [(:require
      [clojure.java.io :refer [make-parents]]
      [clojure.spec.alpha :as spec]
      [clojure.string :as str]
      [erv.utils.conversions :as conv])]
    :cljs
    [(:require
      [clojure.spec.alpha :as spec]
      [clojure.string :as str]
      [erv.utils.conversions :as conv]
      [goog.string :refer [format]]
      goog.string.format)]))

(comment
  "
! meanquar.scl
!
1/4-comma meantone scale. Pietro Aaron's temperament (1523)
 12
!
 76.04900
 193.15686
 310.26471
 5/4
 503.42157
 579.47057
 696.57843
 25/16
 889.73529
 1006.84314
 1082.89214
 2/1

")

(spec/def :meta/scale #{:cps :meru})
(spec/def ::meta (spec/keys :req-un [:meta/scale]))
(comment (spec/explain ::meta (:meta (cps/make 2 [1 3 5 7]))))

(spec/def ::scale (spec/coll-of map?))
(spec/def ::scale-data (spec/keys :req-un [::meta ::scale]))
(comment (spec/explain ::scale-data (cps/make 2 [1 3 5 7])))

(def made-with "Made with the \"erv\" library: https://github.com/diegovdc/erv")

(defn get-cps-meta-description [scale-data]
  (let [{:keys [period]} (:meta scale-data)
        size (:cps/size (:meta scale-data))
        factors (:cps/factors (:meta scale-data))]
    {:name (format "cps-%s_%s-%s_p%s.scl"
                   size
                   (count factors)
                   (str/join "_" factors)
                   period)
     :description (format "A %s CPS scale with factors {%s} and period %s."
                          (str size ")" (count factors))
                          (str/join ", " factors)
                          period)}))

(defn get-meru-meta-description [scale-data]
  (let [{:keys [period]} (:meta scale-data)
        {:keys [size seed total-triads]} (:meta scale-data)]
    {:name (format "meru-%s_seed_%s_triads_%s_p%s.scl"
                   size
                   (str/join "-" seed)
                   total-triads
                   period)
     :description (format "A meru scale of size: %s and  period %s."
                          size
                          period)}))

(defn get-standard-description [scale-data]
  (let [{:keys [scl/name scl/description scl/footer]} (:meta scale-data)]
    {:name name
     :description description
     :footer footer}))

(defn get-description-data
  "Get description data from :meta {:scl/name :scl/description :scl/footer}"
  [scale-data]
  (let [scale-type (-> scale-data :meta :scale)
        standard-description? (or  (:scl/name (:meta scale-data))
                                   (:scl/description (:meta scale-data)))]

    (cond
      standard-description? (get-standard-description scale-data)
      (= :cps scale-type) (get-cps-meta-description scale-data)
      (= :meru scale-type) (get-meru-meta-description scale-data)
      :else {:name "unkown.scl" :description ""})))

(defn format-ratio [ratio]
  #?(:clj
     (cond
       (= ratio (int ratio)) (str ratio "/" 1)
       (ratio? ratio) (str ratio)
       :else (conv/ratio->cents ratio))
     :cljs (if (= ratio (int ratio))
             (str ratio "/" 1)
             (conv/ratio->cents ratio))))

(defn +bounded-ratio-str [note]
  (assoc note :bounded-ratio-str (format-ratio (note :bounded-ratio))))

(defn format-scale-for-scl [scale]
  (->> scale
       (map
        #(merge %
                (+bounded-ratio-str
                 (if (-> % :bounded-ratio (== 1))
                   (assoc % :bounded-ratio (% :bounding-period))
                   %))))
       (sort-by :bounded-ratio)))

(defn make-scl-file [scale-data]
  (let [scale (format-scale-for-scl (:scale scale-data))
        size (count scale)
        pitches (str/join "\n" (map :bounded-ratio-str scale))
        {:keys [name description footer]} (get-description-data scale-data)]
    {:filename name
     :content (format "! %s\n!\n%s\n %s\n!\n%s\n%s\n! %s"
                      name
                      description
                      size
                      pitches
                      (if-not footer "" (str "\n! " footer))
                      made-with)}))

(defn ^:export spit-file
  [filepath scale-data]
  #?(:clj
     (do (make-parents filepath)
         (spit filepath (:content (make-scl-file scale-data)))
         (println "SCL file created:" filepath))
     :cljs (throw (js/Error. "Cannot spit file in JS, use make-scl-file instead"))))

;;;;;;;;;;;
;; KBM Files
;;;;;;;;;;;

(def kbm-template
  "Template for a keyboard mapping"
  "! %s
! KBM file for: %s; %s
! Size of map. The pattern repeats every so many keys:
%s
! First MIDI note number to retune:
0
! Last MIDI note number to retune:
127
! Middle note where the first entry of the mapping is mapped to:
%s
! Reference note for which frequency is given:
%s
! Frequency to tune the above note to
%s
! Scale degree to consider as formal octave (determines difference in pitch
! between adjacent mapping patterns):
%s
! Mapping.
! The numbers represent scale degrees mapped to keys. The first entry is for
! the given middle note, the next for subsequent higher keys.
! For an unmapped key, put in an \"x\". At the end, unmapped keys may be left out.
%s
! %s
")

(defn make-kbm
  [{:as _kbm-template-config
    :keys [file-name scale-data degrees middle-note middle-note-freq comments?]
    :or {middle-note-freq (conv/midi->cps 60)
         middle-note 60
         comments? true}}]
  (let [scale-description (get-description-data scale-data)
        scale (:scale scale-data)
        scale-size (count scale)
        degrees* (->> degrees
                      (map #(mod % scale-size))
                      sort
                      (str/join "\n"))]
    (cond-> (format kbm-template
                    file-name
                    (:name scale-description "unknown.scl")
                    (:description scale-description "")
                    (count degrees)
                    middle-note      ;; middle note
                    middle-note      ;; reference note
                    middle-note-freq ;; frequency
                    scale-size
                    degrees*
                    made-with)
      (not comments?) ((fn [kbm]
                         (let [lines (str/split-lines kbm)]
                           (->> lines
                                (remove #(str/starts-with? % "!"))
                                (str/join "\n"))))))))

(comment
  (require '[erv.utils.ratios :refer [ratios->scale]]
           '[erv.cps.core :as cps])
  (def scale-data (cps/make 2 [1 3 5 7]))

  (println (make-kbm {:scale-data scale-data
                      :comments? false
                      :degrees [0 6 12 16 22 28 32]}))

  (println (make-kbm {:scale-data {:scale (ratios->scale [4181/4096
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
                                                          2/1])}
                      :comments? false
                      :degrees [0 6 12 16 22 28 32]}))

  (spit-kbm {:scale-data scale-data
             :degrees [1 3]}))

(defn ^:export spit-kbm
  #_{:clj-kondo/ignore [:unused-binding]}
  [{:keys [filepath] :as kbm-template-config}]
  #?(:clj
     (do (make-parents filepath)
         (spit filepath (make-kbm (assoc kbm-template-config
                                         :file-name (-> filepath (str/split #"/") last))))
         (println "Creating kbm file at:" filepath))
     :cljs (throw (js/Error. "Cannot spit file in JS, use make-kbm instead"))))
