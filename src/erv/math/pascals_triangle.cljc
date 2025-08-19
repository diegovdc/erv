(ns erv.math.pascals-triangle)

(defn make
  ([size] (make 1 1 size))
  ([seed-l seed-r size]
   (reduce (fn [acc _]
             (->> (concat [seed-l]
                          (mapv #(apply + %)
                                (into [] (partition 2 1 (last acc))))
                          [seed-r])
                  (map #?(:clj bigint :cljs js/BigInt))
                  (into [])
                  (conj acc)))
           [[seed-r]] ;; seed-r is privileged so that it works well for calculating the meru diagonals (as shown in merutwo.pdf)
           (range size))))

(comment
  (make 100))

(defn row [n] (last (make n)))

(defn factorial [x]
  (apply * (map #?(:clj bigint :cljs js/BigInt)
                (range 1 (inc x)))))

(defn default-coord-map
  "Calculates a point the pascal's triangle based on an `x,y` coordinate.
  Taken from Thomas M. Green's Recurrent Sequences and Pascal's Triangle (referred in meruone.pdf)."
  [[x y]]
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

(do
  (defn make-coord-map
    "Returns a `hash-map` that maps between a pascal coordinate (a [pos-int? pos-int?] vector) and the corresponding pascal-number. Works the same as `default-coord-map` (except for the row `size` constraint) but works for custom seeded pascal triangles. "
    [seed-l seed-r size]
    (->> (map vector
              (apply concat (pascal-coordinates size))
              (apply concat (make seed-l seed-r size)))
         (into {})))
  (make-coord-map 1 2 3))
