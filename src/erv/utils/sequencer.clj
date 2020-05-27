(ns erv.utils.sequencer
  (:require
   [overtone.core :as o]
   [time-time.sequencing-2 :as sequencer]))

;;;  TODO remove dependency from time-time
(def schedule! sequencer/schedule!)

(o/defsynth sin
  [freq 400
   dur 1
   porta 0
   porta-amp 0.3
   porta-amp-speed 0.3
   amp 1
   pan 0]
  (-> (o/sin-osc:ar (o/lag:kr freq porta)
                    0
                    (o/lag:kr porta-amp porta-amp-speed))
      (* amp (o/env-gen:kr (o/envelope
                            [0 1 0.1 0]
                            [0.1 dur 0.3]
                            :lin)
                           :action o/FREE))
      (o/pan2 pan)
      (->> (o/out 0))))

(o/defsynth tri
  [freq 400
   dur 1
   porta 0
   porta-amp 0.3
   porta-amp-speed 0.3
   amp 1
   pan 0]
  (-> (o/lf-tri:ar (o/lag:kr freq porta))
      (* amp (o/env-gen:kr (o/envelope
                            [0 1 0.1 0]
                            [0.1 dur 0.3]
                            :lin)
                           :action o/FREE))
      (o/pan2 pan)
      (->> (o/out 0))))

;; TODO fix dur is bpm or something
(defn play! [freqs dur & {:keys [on-event] :or {on-event (fn [index freq])}}]
  (sequencer/schedule!
   (atom {:f (fn [{:keys [data]}]
               (let [i (data :index)
                     dur (-> data :durs (nth i))
                     freq (nth freqs i)]
                 (try
                   (do
                     (on-event i freq)
                     (tri :freq freq
                          :dur dur
                          :amp 0.3))
                   (catch Exception e (println e)))))
          :started-at (+ 1 (o/now))
          :next-event (+ 1 (o/now))
          :elapsed-at 0
          :durs (repeat (count freqs) 1)
          :index 0
          :ratio 1
          :tempo dur
          :loop? false
          :playing? true})))
