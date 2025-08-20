(ns erv.meru.diagonals
  "Based on: https://www.anaphoria.com/meru.pdf"
  (:require [erv.math.pascals-triangle :as pascals-triangle]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; V3
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- slope->n-increment ;; TODO rename
  [{:keys [x _y] :as _slope}]
  (/ 1 x))

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
                             :convergence-ratio ratio
                             :last-10 (take 10 (conj last-10 ratio))
                             :convergence-index (inc convergence-index))))))
                  {:convergence-ratio nil
                   :convergence-index -1
                   :last-10 ()
                   :series [(first (first parts))]}
                  parts)))
       (#(dissoc % :last-10))))

(defn intish? [n] (= n (int n)))

(defn get-x
  "x = (slope-x/slope-y) * (y - n)
  NOTE: Multiplied by -1 because the line is assumed to be descending."
  [y n slope]
  (* -1 (/ (:x slope) (:y slope)) (- y n)))

(defn get-y
  "y = (slope-y/slope-x)*x + n
  NOTE: Multiplied by -1 because the line is assumed to be descending."
  [x n slope]
  (+ n (* x -1 (/ (:y slope) (:x slope)))))

(defn make-diagonal
  "Given the linear formula `y = (slope-y/slope-x)*x + n`, the algorithm
  first calculates the crossing at `x` (when `y` is 0). This gives the
  range of `x` integer points to check. Given that range use the line
  formula to find all `y` points that are also integers.
  When both `x` and `y` are integers the coordinate belongs to the pascal diagonal.
  `n-inc-size` is the space between each diagonal, and the `diagonal-index` serves to calcualte the resulting diagonal given the `n-inc-size`."
  [slope n-inc-size diagonal-index]
  (let [n (* diagonal-index n-inc-size)
        x-at-y0  (get-x 0 n slope)
        x-range (range (-> x-at-y0 int inc))]
    (keep (fn [x] (let [y (get-y x n slope)]
                    (when (intish? y) {:x x :y y})))
          x-range)))

#_(make-diagonal {:x 1 :y 2} 1 4)

(do
  (defn diagonals
    [size slope pascal-coord->number]
    (->> (range size)
         (map #(make-diagonal slope (slope->n-increment slope) %))
         (map (fn [coords]
                {:value (->> coords
                             (map  (fn [{:keys [x y]}]
                                     (pascal-coord->number [x y])))
                             (apply +))
                 :coords (vec coords)}))
         convergence-analysis))

  (diagonals 300 {:x 3 :y 5} pascals-triangle/default-coord-map))
