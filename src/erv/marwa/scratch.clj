(ns erv.marwa.scratch
  (:require [clojure.set :as set]))

(comment
  ;; Are all the marwa patterns derived from mos?
  ;; How do they relate to one another?
  )

(def x
  "From http://www.anaphoria.com/MarwaPurviwithin12.pdf"
  [[5 5 5 5 5 5 6]
   [4 6 5 5 5 5 6]
   [4 5 6 5 5 5 6]
   [4 5 5 6 5 5 6]
   [4 5 5 5 6 5 6]
   [5 4 6 5 5 5 6]
   [5 4 5 6 5 5 6]
   [5 4 5 5 6 5 6]
   [5 5 4 6 5 5 6]
   [5 5 4 5 6 5 6]
   [5 5 5 4 6 5 6]])

(def x1
  "Own results (this should be preppened: `[5 5 5 5 5 5 6]`, but is used as the generator)"
  [[4 6 5 5 5 5 6]
   [5 4 6 5 5 5 6]
   [5 5 4 6 5 5 6]
   [5 5 5 4 6 5 6]
   [4 5 6 5 5 5 6]
   [4 5 5 6 5 5 6]
   [4 5 5 5 6 5 6]
   [5 4 5 6 5 5 6]
   [5 4 5 5 6 5 6]
   [5 5 4 5 6 5 6]
   [4 6 4 6 5 5 6]
   [5 4 6 4 6 5 6]
   [4 5 6 4 6 5 6]
   [4 6 5 4 6 5 6]
   [4 6 4 5 6 5 6]])

(set/difference (set x1) (set x))
