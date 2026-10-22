(ns erv.edo.chords
  (:require
   [clojure.set :as set]
   [erv.utils.scale :refer [scale-steps->degrees]]))

(do
  (defn chords-in-pattern
    "Returns the chords in the pattern in two representations:
  The `:chromatic/degrees` which reference the scale at the lowest step level and
  the `:scale/degrees` which reference the actual degrees of a scale of length (count scale-steps)
  The first element of the chord in each `:degree` key is the root of the chord."
    [scale-steps chord-steps]
    (let [total-tones (apply + scale-steps)
          chromatic-degs (scale-steps->degrees scale-steps)
          chromatic->scale-degs (->> chromatic-degs
                                     (mapv (fn [i ch-deg] [ch-deg i])
                                           (range))
                                     (into {}))
          scale-degs-set (set chromatic-degs)
          chord-degrees (map
                         (fn [i]
                           (let [chromatic-degrees (mapv #(mod (+ % i) total-tones) (scale-steps->degrees chord-steps))]
                             {:chromatic/degrees chromatic-degrees
                              :scale/degrees (mapv chromatic->scale-degs chromatic-degrees)}))
                         chromatic-degs)]
      (filter #(set/subset? (:chromatic/degrees %) scale-degs-set) chord-degrees)
      #_[scale-degs chord-degrees]))

  (chords-in-pattern [1 1 2 1 1 2 1 2] [4 4 3]))

(do
  (defn chord-of-degree
    [degree scale-steps chord-steps])
  (chord-of-degree 0 [2 2 1 2 2 2 1] [4 3 5]))
