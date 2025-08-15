(ns erv.meru.diagonals
  "Based on: https://www.anaphoria.com/meru.pdf"
  (:require
   [erv.math.pascals-triangle :as pascals-triangle]))

(defn- diagonals-x-roots
  "Calculates the row indexes where a diagonal should start so that every member of any row will be part of a diagnonal."
  [diagonal-vector]
  (let [[vec-x _vec-y] diagonal-vector]
    (range 0 vec-x 1)))

(diagonals-x-roots [3 1])

(do
  ;; TODO page 3 can't be completely generated at the moment.
  ;; If x in the diagonal is > 1 then there will be some cells that will never be touched what Erv seems to do is to also start diagonals from there, in the order of the row. The zeros that he adds correspond to missing/placeholder values when the row size is < x.

  (defn make
    "NOTE: The `diagonal-vector` is a trigonometric vector with x,y coordinates.   "
    ([slope] (make (pascals-triangle/make 30) slope))
    ([triangle slope]
     (let [[vec-x vec-y] slope
           [x-root & x-roots*] (diagonals-x-roots slope)
           diagonals (loop [y-root 0
                            [x* y*] [0 x-root]
                            diagonal []
                            diagonals []
                            remaining-roots x-roots*]
                       (let [val (-> triangle (nth y* nil) (nth x* nil))]
                         (cond
                           ;; continue with the diagonal
                           val
                           (recur y-root
                                  [(+ x* vec-x) (- y* vec-y)]
                                  (conj diagonal val)
                                  diagonals
                                  remaining-roots)

                           ;; move to next-x-root
                           (and (not val)
                                (seq remaining-roots))
                           (let [[next-x-root & remaining-x-roots*] remaining-roots]
                             (recur y-root
                                    [next-x-root y-root]
                                    []
                                    (conj diagonals diagonal)
                                    remaining-x-roots*))

                           ;; go to next row
                           (and (not val) (nth triangle (inc y-root) nil))
                           (recur (inc y-root)
                                  [0 (inc y-root)]
                                  []
                                  (conj diagonals diagonal)
                                  x-roots*)
                           :else (conj diagonals diagonal))))]
       (mapv (partial apply +) diagonals))))

  (comment)
  ;; pg 13, the order of numbers here does not correspond to the Erv's, his ordering is related to the recurrent sequence formula.
  (make [2 3]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; V2
;; This one really works!
;;;;;;;;;;;;;;;;;;;;;;;;

(defn- slope->n-increment ;; TODO rename
  [{:keys [x _y] :as _slope}]
  (/ 1 x))

(defn- sum-diagonal
  "`inital-val` {:value 0 :slope slope :coords []}"
  [initial-val diagonal]
  (reduce (fn [acc {:keys [coord]}]
            (let [{:keys [x y]} coord
                  pascal-num (pascals-triangle/f x y)]
              (-> acc
                  (update :coords conj coord)
                  (update :value + pascal-num))))
          initial-val
          diagonal))

(defn- diagonal-sums
  [diagonals slopes]
  (mapv (fn [slope]
          (let [initial-val {:value 0 :slope slope :coords []}
                diagonal (get diagonals slope)]
            (if diagonal
              (sum-diagonal initial-val diagonal)
              initial-val)))
        slopes))

(defn- safe-division
  ([a b] (safe-division 0 a b))
  ([default-val a b]
   (if (zero? a) default-val (double (/ b a)))))

(defn- convergence-analysis
  [diagonals-series]
  (->> diagonals-series
       (partition 2 1)
       ((fn [parts]
          (reduce (fn [{:keys [last-10 convergence-index series] :as acc} [a b]]
                    (let [ratio (safe-division nil (:value a) (:value b))]
                      (if (and (= 10 (count last-10))
                               (apply = last-10))
                        (reduced (-> acc
                                     (update :convergence-index - 10)
                                     (assoc :reached-convergence? true)))
                        (-> acc
                            (assoc
                             :series (conj series (assoc b :ratio-vs-previous ratio))
                             :last-10 (take 10 (conj last-10 ratio))
                             :convergence-index (inc convergence-index))))))
                  {:convergence-index -1
                   :last-10 ()
                   :series [(first parts)]}
                  parts)))
       (#(dissoc % :last-10))))

(do
  ;; TODO: maybe make it dynamic so it creates as many rows as necessary instead of having a hardcoded value of 100
  (defn diagonal-sums-data
    "Figure out `n` for every point for the linear formula: y = (slope-y/slope-x)*x + n, by iterating over the pascal-triangle as a vector of coordinates."
    ([slope] (diagonal-sums-data 100 slope))
    ([triangle-rows-size slope]
     (let [n (fn [x y] (+ y (* x (/ (:y slope) (:x slope)))))
           triangle-coords (apply concat (pascals-triangle/pascal-coordinates triangle-rows-size))
           coord->slope (mapv (fn [[x y]] {:coord {:x x :y y} :slope (n x y)}) triangle-coords)
           diagonals (group-by :slope coord->slope)
           last-n (->> diagonals vec (sort-by first) last first)
           n-increment (slope->n-increment slope)
           slopes (range 0 (+ last-n n-increment) n-increment)
           ;; TODO: allow passing in a custom pascal-triangle
           diagonal-sums* (diagonal-sums diagonals slopes)]
       (convergence-analysis diagonal-sums*))))

  (diagonal-sums-data 100 {:x 4 :y 3}))
