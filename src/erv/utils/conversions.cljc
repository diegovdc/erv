(ns erv.utils.conversions
  (:require [erv.utils.core :refer [round2]]))

(defn ratio->cents [ratio]
  (-> (Math/log ratio) (/ (Math/log 2)) (* 1200)))

(defn cps->midi [cps]
  (-> (/ cps 220)
      Math/log
      (/ (Math/log 2))
      (* 12)
      (+ 57)))

(defn midi->cps [midi]
  (-> midi
      (- 57)
      (/ 12)
      (* (Math/log 2))
      Math/exp
      (* 220)))

(def ^:private note-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn- get-octave [midi]
  (-> midi (/ 12) (- 2) int))

(defn midi->name [midi]
  (let [midi* (Math/round (float midi))
        octave (get-octave midi)]
    (str (nth note-names (mod midi* 12))
         octave)))

(defn midi->name*
  "If note is not exactly in 12ET it adds a suffix to the note with the upwards deviation in cents.
  i.e. 60.4 -> C3+40"
  [midi]
  (let [deviation (-> (mod midi 1)
                      (->> (round2 2))
                      (* 100)
                      int)
        octave (get-octave midi)]
    (str (nth note-names (mod (int midi) 12))
         octave
         (when-not (= 0 deviation) (str "+" deviation)))))

(defn cps->name [cps] (-> cps cps->midi midi->name))

(defn cps->name*
  "If note is not exactly in 12ET it adds a suffix to the note with the upwards deviation in cents.
  i.e. 441 -> A.04"
  [cps]
  (-> cps cps->midi midi->name*))

(comment
  (midi->name* 60.1)
  (cps->name* 444))
