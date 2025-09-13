(ns erv.meru.diagonals
  "Based on: https://www.anaphoria.com/meru.pdf"
  (:require
   [erv.math.pascals-triangle :as pascals-triangle]
   [erv.meru.utils :refer [get-convergence-double-with-precision]]
   [erv.mos.v3.core :refer [gen->mos-ratios]]
   [erv.utils.core :refer [round2]]
   [taoensso.timbre :as timbre]))

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

(defn- default-convergence?-fn
  [last-10-ratios]
  (and (= 10 (count last-10-ratios))
       (apply = last-10-ratios)))

(defn- decimal-places-convergence?-fn
  "Evaluate convergence according to a given number of decimal places in the provided ratios."
  [decimal-places last-10-ratios]
  (->> last-10-ratios
       (map #(if (nil? %) nil (round2 decimal-places %)))
       default-convergence?-fn))

(decimal-places-convergence?-fn 2 [1.111234
                                   1.111235])

(defn- convergence-analysis
  ([diagonals-series] (convergence-analysis default-convergence?-fn diagonals-series))
  ([convergence?-fn diagonals-series]
   (->> diagonals-series
        (partition 2 1)
        ((fn [parts]
           (reduce (fn [{:keys [last-10 convergence-index series-data] :as acc} [a b]]
                     (let [ratio (safe-division nil (:value a) (:value b))]
                       (if (convergence?-fn last-10)
                         (reduced (-> acc
                                      (update :convergence-index - 10)
                                      (assoc :reached-convergence? true)))
                         (-> acc
                             (assoc
                              :series-data (conj series-data (assoc b :ratio-vs-previous ratio))
                              :convergence-double ratio
                              :last-10 (take 10 (conj last-10 ratio))
                              :convergence-index (inc convergence-index))))))
                   {:convergence-double nil
                    :convergence-index -1
                    :last-10 ()
                    :series-data [(first (first parts))]
                    :reached-convergence? false}
                   parts)))
        (#(dissoc % :last-10))
        (#(assoc % :series (mapv :value (:series-data %)))))))

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

(defn diagonals
  [{:keys [size slope pascal-coord->number convergence?-fn convergence-precision]
    :or {pascal-coord->number pascals-triangle/default-coord-map}}]
  (when (and convergence-precision convergence?-fn)
    (timbre/warn "Both `convergence?-fn` and `convergence-precision` have been provided. The latter is going to be ignored."))
  (let [convergence?-fn (cond
                          convergence?-fn convergence?-fn
                          convergence-precision (partial decimal-places-convergence?-fn convergence-precision)
                          :else default-convergence?-fn)
        update-convergence-data (fn [data]
                                  (assoc data
                                         :triangle-seed (if (= pascal-coord->number pascals-triangle/default-coord-map)
                                                          {:left 1 :right 1}
                                                          (:triangle-seed (meta pascal-coord->number)))
                                         :convergence-precision convergence-precision
                                         :convergence-double-with-precision (get-convergence-double-with-precision
                                                                             convergence-precision
                                                                             (:convergence-double data))))]
    (->> (range size)
         (map #(make-diagonal slope (slope->n-increment slope) %))
         (map (fn [coords]
                {:value (->> coords
                             (map  (fn [{:keys [x y]}]
                                     (pascal-coord->number [x y])))
                             (apply +))
                 :coords (vec coords)}))
         (convergence-analysis convergence?-fn)
         update-convergence-data)))
