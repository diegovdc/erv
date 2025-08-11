(ns erv.math.pascals-triangle)

(defn make
  [size]
  (reduce (fn [acc _]
            (->> (concat [1]
                         (mapv #(apply + %) (partition 2 1 (last acc)))
                         [1])
                 (into [])
                 (conj acc)))
          [[1]]
          (range size)))

(comment
  (make 10))

(defn row [n] (last (make n)))
