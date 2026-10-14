(ns erv.meru.utils
  (:require
   [erv.utils.core :refer [round2]]))

(defn get-convergence-double-with-precision
  [convergence-precision convergence-double]
  (if convergence-precision
    (round2 convergence-precision
            convergence-double)
    convergence-double))
