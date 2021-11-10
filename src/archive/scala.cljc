(ns archive.scala
  "Parse the scala archive: https://huygens-fokker.org/docs/scalesdir.txt,
  https://huygens-fokker.org/scala/downloads.html, https://huygens-fokker.org/docs/scales.zip"
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [erv.utils.conversions :as conv]))

(defn get-scl-files []
  (->> (file-seq (io/file "./src/archive/huygens-fokker-scala-archive"))
       (remove #(.isDirectory %))
       (map #(slurp (.getPath %)))))

(def source "https://huygens-fokker.org/docs/scales.zip")

(defn get-title [scl]
  (some-> scl
          (str/split #"\n")
          first
          (str/split #"!")
          second
          str/trim))
(defn get-description [scl]
  (some-> scl
          (str/split #"\n")
          (->> (drop-while #(= \! (first %))))
          first
          str/trim))
(comment
  (def test-scale "!
0A440Lucy06Tuned 1b4s RootKeyA = CC#DD#EFF#GG#ABbB
 12
!
 122.53517
 190.98593
 313.52110
 381.97187
 504.50703
 572.95780
 695.49297
 818.02813
 886.47890
 1009.01407
 1077.46483
 2/1")
  (get-title test-scale)
  (get-description test-scale))

;;;;;;;;;;;;;
;;; Parse
;;;;;;;;;;;;;

(defn parse-scale [scl]
  (-> scl (str/split #"\n")
      (->>
       (remove #(= \! (first %)))       ; remove comment lines
       (drop 1)                         ; drop description
       ;;  get all numbers
       (map #(-> % str/trim (str/split #" ") first read-string))
       (filter number?)
       (drop 1))))                    ; drop scale size


(defn- get-ratio [num]
  (if (or (int? num) (ratio? num))
    num
    (conv/cents->ratio num)))

(defn get-scale [scl]
  (let [scale (parse-scale scl)
        period (last scale)
        ratios* (map get-ratio scale)
        ;; remove the period (last item) and add a 1 at the front
        ratios (conj (drop-last 1 ratios*) 1)]
    (map (fn [ratio] {:bounded-ratio ratio
                     :bounding-period period})
         ratios)))

(comment (parse-scale test-scale))

(defn parse-file [scl]
  (try
    (let [scale (get-scale scl)]
      {:meta {:scale :scala-archive
              :source source
              :size (count scale)
              :filename (get-title scl)
              :description (get-description scl)}
       :scale scale})
    (catch Exception e (println "could not read "
                                (-> scl (str/split #"\n")
                                    (->> (take 4)))))))
;;;;;;;;;;;
;;; Write
;;;;;;;;;;;

(defn keyword->camel [kw]
  (let [[head & rest*] (-> (name kw)
                           (str/split #"-"))]
    (apply str head (map str/capitalize rest*))))

(defn parse-files []
  (->> (get-scl-files)
       (remove nil?)
       (map parse-file)))

(defn write-archive-json [path]
  (->> (parse-files)
       (#(json/write-str % :key-fn keyword->camel))
       (spit path)
       ((constantly :done))))

(defn write-archive-edn [path]
  (->> (parse-files)
       (pr-str)
       (spit path)
       ((constantly :done))))

(comment
  (write-archive-json "src/archive/scala-archive.json")
  (write-archive-edn "src/archive/scala-archive.edn")
  (clojure.edn/read-string (slurp "src/archive/scala-archive.edn")))
