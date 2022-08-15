(ns erv.math.pascals-triangle)

(defn make [size]
  (reduce (fn [acc _]
            (conj acc
                  (concat [1]
                          (mapv #(apply + %) (partition 2 1 (last acc)))
                          [1])))
          [[]]
          (range size)))

(defn row [n] (last (make n)))
