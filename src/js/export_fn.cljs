(ns js.export-fn
  (:require
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.mos.mos :as mos]
   [erv.utils.conversions :as conv]
   [erv.utils.core :as utils]))

(defn generate-exports []
  (clj->js {:cps {:make (comp clj->js cps/make)}
            :mos {:make (comp clj->js mos/make)}
            :edo {:fromPattern (comp clj->js edo/from-pattern)}
            :utils {:rotate (comp clj->js utils/rotate)
                    :ratioToCents (comp clj->js conv/ratio->cents)}}))
