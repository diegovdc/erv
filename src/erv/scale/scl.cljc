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
         (spit filepath (:content (make-scl-file scale-data))))
     :cljs (throw (js/Error. "Cannot spit file in JS, use make-scl-file instead"))))
