(ns sin
  (:require [overtone.core :as o]))

(o/defsynth sin*
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
                            [0.1 dur 0.3 ]
                            :lin)
                           :action o/FREE))
      (o/pan2 pan)
      (->> (o/out 0))))


(comment (o/stop))
