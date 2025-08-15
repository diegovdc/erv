(ns erv.math.pascals-triangle)

(defn make
  [size]
  (reduce (fn [acc _]
            (->> (concat [1]
                         (mapv #(apply + %)
                               (into [] (partition 2 1 (last acc))))
                         [1])
                 (map #?(:clj bigint :cljs js/BigInt))
                 (into [])
                 (conj acc)))
          [[1]]
          (range size)))

(comment
  (make 100))

(defn row [n] (last (make n)))

(defn factorial [x]
  (apply * (map #?(:clj bigint :cljs js/BigInt)
                (range 1 (inc x)))))

(defn f
  "Calculates a point the pascal's triangle based on an `x,y` coordinate.
  Taken from Thomas M. Green's Recurrent Sequences and Pascal's Triangle (referred in meruone.pdf)."
  [x y]
  (/ (factorial (+ x y))
     (* (factorial x) (factorial y))))

;; TODO create a Pascal's Triangle implementation that can be seeded, and that returns something that has an interface like `f` above.

(defn pascal-coordinates
  [size]
  (->> (range size)
       (mapv
        (fn [size*]
          (->> (range 0 (inc size*))
               (map
                (fn [i] [(- size* i) i])))))))
