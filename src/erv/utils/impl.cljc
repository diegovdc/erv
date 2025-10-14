(ns erv.utils.impl
  "Contains implementations or functions that are shared among different files.")

(defn +degree
  "Adds degrees to a scale"
  [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))
