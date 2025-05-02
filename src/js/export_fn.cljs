(ns js.export-fn
  (:require
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.mos.mos :as mos]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [erv.utils.ratios :as ratios]
   [erv.utils.core :as utils]))

(defn generate-exports []
  (clj->js {:cps {:make (comp clj->js cps/make)}
            :mos {:make (comp clj->js mos/make)}
            :edo {:fromPattern (comp clj->js edo/from-pattern)}
            :utils {:rotate (comp clj->js utils/rotate)
                    :ratioToCents (comp clj->js conv/ratio->cents)
                    :centsToRatio (comp clj->js conv/cents->ratio)
                    :freqToMidi (comp clj->js conv/cps->midi)
                    :ratiosToScale (comp clj->js
                                         (fn [period ratios]
                                           (ratios/ratios->scale period (js->clj ratios))))}
            :scale {:degToFreq  (comp clj->js (fn [scale root degree]
                                                (let [scale* (js->clj scale {:keywordize-keys true})]
                                                  (scale/deg->freq scale*  root degree))))}}))
