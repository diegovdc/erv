(ns theory.chords
  (:require [clojure.set :as set]))

(defn- conj+ [nums n]
  (conj nums (+ (or (last nums) 0) n)))
(do
  (defn intervals->degrees
    ([intervals] (intervals->degrees intervals 0))
    ([intervals transposition]
     (let [scale-size (apply + intervals)]
       (->> intervals
            drop-last
            (reduce conj+ [0])
            (map #(-> %
                      (+ transposition)
                      (mod scale-size)))))))
  (intervals->degrees [2 2 1 2 2 2 1] 2))

(do
  (defn edo-chords [intervals chord-shapes-set]
    (let [scale-size (apply + intervals)
          degs (intervals->degrees intervals)
          degs-set (set degs)]
      (->> degs
           (mapcat (fn [deg]
                     (map (fn [chord-shape]
                            {:chord chord-shape
                             :degrees (->> chord-shape
                                           (reduce conj+ [deg])
                                           drop-last
                                           (map #(mod % scale-size)))})
                          chord-shapes-set)))
           (filter #(let [chord (set (:degrees %))]
                      (= chord (set/intersection degs-set chord)))))))
  #_(edo-chords [2 2 1 2 2 2 1] #{[4 3 5] [3 4 5] [3 3 6]}))

(def edo-12-names
  {0 "C", 1 "C#", 2 "D", 3 "D#", 4 "E", 5 "F", 6 "F#"
   7 "G", 8 "G#", 9 "A", 10 "A#", 11 "B"})

(defn name-degrees [names-map degrees]
  (map names-map degrees))

(defn +notes [names-map chords]
  (map #(assoc % :notes (name-degrees names-map (:degrees %))) chords))

(comment
  (map (partial name-degrees edo-12-names)
       (edo-chords [1 2 1 2 1 2 1 2] #{[4 3 5] [3 4 5] [3 3 6]}))

  (map (partial name-degrees edo-12-names)
       (edo-chords [1 1 3 1 1 2 3] #{[4 3 5] [3 4 5] [3 3 6]}))

  (map (partial name-degrees edo-12-names)
       (edo-chords [2 1 1 2 1 1 2 1 1] #{[4 3 5] [3 4 5] [3 3 6]}))

  (->> (edo-chords [2 1 1 2 1 1 2 1 1]
                   #{[4 3 3 2]
                     [4 3 4 1]
                     [3 4 3 2]
                     [3 4 4 1]
                     [5 5 2]})
       (+notes edo-12-names)
       (group-by :chord)
       (map (juxt first
                  (comp #(map :notes %) second)))))
