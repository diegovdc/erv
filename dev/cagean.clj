(ns cagean
  "Inspired by John Cage's Number pieces"
  (:require
   [overtone.core :refer [now]]
   [erv.utils.conversions :as conv]
   [erv.cps.core :as cps]
   [time-time.sequencing-2 :as sequencer]
   [sin]))

(defonce voice (atom {:f (fn [{:keys [data voice]}]
                           (sin/sin* :freq 500)
                           (swap! voice assoc :durs [(rand-nth [1/4 1])]))
                      :started-at (+ 1000 0 (now))
                      :next-event nil
                      :elapsed-at 0
                      :durs [1 1/4 1/4 1/4 1/4]
                      :index 0
                      :ratio 1
                      :tempo 1000
                      :loop? true
                      :playing? true}))

(swap! voice assoc :f (fn [{:keys [data voice]}]
                        (let [dur (rand 10)]
                          (sin/sin* :freq 500)
                          (swap! voice assoc :durs [dur]))))
(declare get-freq get-durs)

(defn wrap-at [i coll]
  (let [i* (mod i (count coll))]
    (nth coll i*)))

(defn make-voices [n]
  (map (fn [_] (atom {:f (fn [{:keys [data voice]}]
                          (let [durs (get-durs data voice)
                                i (data :index)]
                            (sin/tri* :freq (get-freq i)
                                      :dur (wrap-at i durs)
                                      :amp (rand 0.3))
                            (swap! voice assoc :durs [(rand-nth durs)])))
                     :started-at (+ 1000 0 (now))
                     :next-event nil
                     :elapsed-at 0
                     :durs [1 1/4 1/4 1/4 1/4]
                     :index 0
                     :ratio 1
                     :tempo 1000
                     :loop? true
                     :playing? true}))
       (range 0 n)))

(defn play! [voice]
  (swap! voice #(merge % {:loop? true
                          :elapsed-at 0
                          :started-at (+ 1000 0 (now))
                          :next-event (+ 1000 (now))
                          :playing? true}))
  (sequencer/schedule! voice)
  {:voice voice :stop (fn [] (swap! voice #(assoc % :loop? false :playing? false)))})

(def voices (make-voices 5))

(comment
  (doseq [v voices] (play! v)))

(def hexany
  (->> [11 13 27 17]
       (cps/->cps 2)
       cps/set->maps
       (cps/bound-ratio 3/2)
       (cps/maps->data :bounded-ratio)))

(def hexany-2
  (->> [9 3 5 7]
       (cps/->cps 2)
       cps/cps-intervals-by-denominator
       (#(cps/set-chord % #{9 3} 3))
       keys
       cps/set->maps
       (cps/bound-ratio 2)
       (cps/maps->data :bounded-ratio)))

(comment (->> hexany :scale ))

(defn get-freq [index]
  (->> hexany-2 :scale
       #_(wrap-at index)
       rand-nth
       :bounded-ratio
       (* 200 (rand-nth [1 2 1/2 1/4 4]))))

(defn get-durs [data voice]
  [(rand 10)])

(comment
  (doseq [v voices]
    (swap! v #(assoc % :loop? false :playing? false))))

(defn stop! []
  (doseq [v voices]
    (swap! v #(assoc % :loop? false :playing? false))))

(comment (stop!)
         (overtone.core/stop))

(comment
  (overtone.core/recording-start "~/Desktop/hexany-3:2.wav")
  (overtone.core/recording-stop ))


(map (fn [n] (* 880 n)) [1/5 1/3 1/7  1])
