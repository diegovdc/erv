(ns erv.utils.impl
  "Contains implementations or functions that are shared among different files."

  #? (:cljs (:require [goog.string :as gstr]
                      [goog.string.format])))

#?(:cljs
   (def format gstr/format))

(defn +degree
  "Adds degrees to a scale"
  [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))
