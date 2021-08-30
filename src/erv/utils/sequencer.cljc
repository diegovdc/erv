(ns erv.utils.sequencer
  (:require
   #?(:clj [overtone.core :as o]
      :cljs ["tone/build/esm/index" :as Tone])
   [time-time.sequencing-3 :as sequencer]
   [erv.utils.core :refer [wrap-at]]))

#?(:clj (do (o/defsynth sin
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
                  (->> (o/out 0)))))

   :cljs (def synth (.toDestination (Tone/Synth.))))

(defn play! [freqs dur
             & {:keys [on-event]
                :or {on-event (fn [_ _])}}]
  #?(:cljs (Tone/start))

  (let [durs (repeat (count freqs) dur)]
    (sequencer/play!
     durs
     (fn [{:keys [data]}]
       (let [index (data :index)
             freq (wrap-at index freqs)]
         (on-event index freq)
         #?(:clj
            (sin :freq freq
                 :dur dur
                 :amp 0.3)
            :cljs
            (.triggerAttackRelease synth freq dur))))
     :loop? false
     :ratio 0.5)))
