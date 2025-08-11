(ns erv.meru.diagonals
  "Based on: https://www.anaphoria.com/meru.pdf"
  (:require
   [erv.math.pascals-triangle :as pascals-triangle]))

(do
  ;; TODO page 3 can't be completely generated at the moment.
  ;; If x in the diagonal is > 1 then there will be some cells that will never be touched what Erv seems to do is to also start diagonals from there, in the order of the row. The zeros that he adds correspond to missing/placeholder values when the row size is < x.
  (defn make
    ([diagonal] (make (pascals-triangle/make 30) diagonal))
    ([triangle diagonal]
     (let [vec-x (first diagonal)
           vec-y (second diagonal)
           diagonals (loop [initial-row 0
                            coord [0 0]
                            diagonal []
                            diagonals []]
                       (let [[y* x*] coord
                             val (-> triangle
                                     (nth y* nil)
                                     (nth x* nil))]
                         (cond
                           val (recur
                                initial-row
                                [(+ y* vec-y) (+ x* vec-x)]
                                (conj diagonal val)
                                diagonals)
                           (and (not val)
                                (nth triangle (inc initial-row) nil)) (recur
                                                                       (inc initial-row)
                                                                       [(inc initial-row) 0]
                                                                       []
                                                                       (conj diagonals diagonal))
                           :else (conj diagonals diagonal))))]
       (mapv (partial apply +) diagonals)))))

(comment
  (make [2 -1]))
