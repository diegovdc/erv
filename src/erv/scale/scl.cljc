(ns erv.scale.scl
  "The purpose of this namespace is to produce scala files."
  #?@
  (:clj
   [(:require
     [clojure.java.io :refer [make-parents]]
     [clojure.spec.alpha :as spec]
     [clojure.string :as str]
     [erv.meru.core :as meru]
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
     :description (format "A %s CPS scale with factors {%s} and period %s. %s"
                          (str size ")" (count factors))
                          (str/join ", " factors)
                          period
                          made-with)}))

(defn get-meru-meta-description [scale-data]
  (let [{:keys [period]} (:meta scale-data)
        {:keys [size seed total-triads]} (:meta scale-data)]
    {:name (format "meru-%s_seed_%s_triads_%s_p%s.scl"
                   size
                   (str/join "-" seed)
                   total-triads
                   period)
     :description (format "A meru scale of size: %s and  period %s. %s"
                          size
                          period
                          made-with)}))

(defn get-standard-description [scale-data]
  (let [{:keys [scl/name scl/description]} (:meta scale-data)]
    {:name name
     :description description}))

(defn get-description-data [scale-data]
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
        {:keys [name description]} (get-description-data scale-data)]
    {:filename name
     :content (format "! %s\n!\n%s\n %s\n!\n%s" name description size pitches)}))

(defn ^:export spit-file
  [filepath scale-data]
  #?(:clj
     (do (make-parents filepath)
       (spit filepath (:content (make-scl-file scale-data))))
     :cljs (throw (js/Error. "Cannot spit file in JS, use make-scl-file instead"))))

(comment
  (require '[erv.cps.core :as cps]
           '[erv.meru.core :as meru]
           '[erv.edo.core :as edo])
  (edo/from-pattern [4,8,1,4,5,4,8,1,8,1,4,5])
  (cps/make 2 [1 3 5 7] :norm-fac 7)
  (count meru/test1)
  (get-cps-meta-description (cps/make 2 [1 3 5 7]))

  (first meru/test1)
  (format-scale-for-scl (:scale (first meru/test1)))
  (let [index 9
        scl (make-scl-file (first meru/test1) #_(nth meru/test1 index))]
    (spit
      (str "/Users/diego/Music/tunings/"  "22t-" (:filename scl))
      (:content scl)))

  (spit "/home/diego/Desktop/dekany-1-5-7-13-23_p2.scl" (:content (make-scl-file (cps/make 2 [1 5 7 13 23] :norm-gen 115/64))))

  (format-scale-for-scl (:scale (cps/make 2 [1 3 5 7] :norm-fac 35)))
  (format-scale-for-scl (:scale (edo/from-pattern [4,8,1,4,5,4,8,1,8,1,4,5])))
  (spit "/home/diego/Desktop/dekany-1-5-7-13-23_p2.scl" (:content (make-scl-file (cps/make 2 [1 5 7 13 23] :norm-gen 115/64))))
  (spit "/home/diego/Desktop/polydori.scl"
        (:content (make-scl-file (cps/make 4 [1 3 9 19 15 21 7] :norm-fac (* 15 21 19 9)) )))
  (spit "53edo-secondary-mos.scl"
        (:content (make-scl-file (edo/from-pattern [4,8,1,4,5,4,8,1,8,1,4,5]))))

  (spit-file
    "/Users/diego/Music/tunings/7-11.scl"
    {:scale [{:ratio 1, :bounded-ratio 1, :bounding-period 2}
             {:ratio 128/121, :bounded-ratio 128/121, :bounding-period 2}
             {:ratio 8/7, :bounded-ratio 8/7, :bounding-period 2}
             {:ratio 77/64, :bounded-ratio 77/64, :bounding-period 2}
             {:ratio 14/11, :bounded-ratio 14/11, :bounding-period 2}
             {:ratio 11/8, :bounded-ratio 11/8, :bounding-period 2}
             {:ratio 16/11, :bounded-ratio 16/11, :bounding-period 2}
             {:ratio 11/7, :bounded-ratio 11/7, :bounding-period 2}
             {:ratio 128/77, :bounded-ratio 128/77, :bounding-period 2}
             {:ratio 7/4, :bounded-ratio 7/4, :bounding-period 2}
             {:ratio 121/64, :bounded-ratio 121/64, :bounding-period 2}]}))
