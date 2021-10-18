(ns erv.scale.scl
  "The purpose of this namespace is to produce scala files."
  #?@
   (:clj
    [(:require
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

(spec/def :meta/scale #{:cps})
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

(defn get-description-data [scale-data]
  (case (-> scale-data :meta :scale)
    :cps (get-cps-meta-description scale-data)
    {:name "unkown.scl" :description ""}))

(defn format-ratio [ratio]
  #?(:clj
     (if (= ratio (int ratio))
       (str ratio "/" 1)
       (str ratio))
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
                 (if (-> % :bounded-ratio (= 1))
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

(comment
  (require '[erv.cps.core :as cps])
  (cps/make 2 [1 3 5 7] :norm-fac 7)
  (get-cps-meta-description (cps/make 2 [1 3 5 7]))
  (format-scale-for-scl (:scale (cps/make 2 [1 3 5 7] :norm-fac 35)))
  (spit "/home/diego/Desktop/dekany-1-5-7-13-23_p2.scl" (:content (make-scl-file (cps/make 2 [1 5 7 13 23] :norm-gen 115/64)))))
