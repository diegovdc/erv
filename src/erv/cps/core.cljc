(ns erv.cps.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(do
  (defn ->set [size generators]
    (->> (combo/combinations generators size) ;; returns all possible pairs
         (remove (partial apply =))
         (map set)
         set))

  (defn set->maps
    "Creates a hexany-map"
    [hexany-set]
    (->> hexany-set
         (map (fn [pair] {:set pair}))))

  (defn within-bounding-period
    [bounding-intvl ratio]
    {:pre [(> bounding-intvl 1)]}
    (loop [r ratio]
      (cond
        (> r bounding-intvl) (recur (/ r bounding-intvl))
        (< r 1) (recur (* r bounding-intvl))
        (= bounding-intvl r) 1
        :else r)))

  #_(within-bounding-period 2 1/21)

  (defn bound-ratio
;;; TODO, how to be able to specify multiple bounding-periods
    "Calculate all the ratios within the bounding-period (e.g. octave, tritave, etc.)"
    [bounding-period hexany-map]
    (->> hexany-map
         (map (fn [node*]
                (let [ratio (apply * (node* :set))]
                  (assoc node*
                         :ratio ratio
                         :bounded-ratio (within-bounding-period
                                         bounding-period
                                         ratio)
                         :bounding-period bounding-period))))))

  (defn add-edge
    "Adds to the set of edges of `node-from` the value `node-to`"
    [graph node-from node-to]
    (update graph node-from (comp set conj) node-to))

  (defn maps->graph
    "A map that connects each node with all the other nodes that share one elemnt of the set"
    [hexany-maps]
    (loop [nodes (into [] hexany-maps)
           graph {}]
      (if-not (seq nodes)
        graph
        (let [rest* (rest nodes)
              current-node (first nodes)
              edges (->> rest*
                         (filter #(-> (set/intersection (:set current-node)
                                                        (:set %))
                                      seq))
                         (map #(conj [] current-node %)))]
          (recur rest*
                 (reduce
                  (fn [graph [a b]]
                    (-> graph
                        (update a (comp set conj) b)
                        (update b (comp set conj) a)))
                  graph
                  edges))))))

  (defn graph->simple-graph
    "Convert a graph full of data to a graph of only the pairs"
    [hexany-graph]
    (->> hexany-graph
         (map (fn [[k v]]
                [(k :set) (set (map :set v))]))
         (into {})))

  (defn maps->data
    [scale-sort-fn hexany-maps]
    (let [full-graph (maps->graph hexany-maps)]
      {:scale (sort-by scale-sort-fn hexany-maps)
       :nodes hexany-maps
       :graphs {:full full-graph
                :simple (graph->simple-graph full-graph)}}))

  (def hx (->> [1 3 5 7]
               (->set 2)
               set->maps
               (bound-ratio 2)
               (maps->data :bounded-ratio)
               :scale
               (map :bounded-ratio)
               sort
               user/spy))

  #_(defn positions
      [pred coll]
      (keep-indexed (fn [idx x]
                      (when (pred x) idx))
                    coll)))

#_(let [ref-node #{3 5}
        intval -3
        direction (if (> 0 intval) -1 1)
        node-index (first (positions #(= (:set %) ref-node) (hx :scale)))
        next-node-index (mod (+ node-index intval) (count (hx :scale)))
        next-pair (:set ((vec (hx :scale)) next-node-index))
        ratio (apply * next-pair)
        bound-multiple (quot (+ (* direction next-node-index) intval) (count (hx :scale)))
        bound-multiple* (cond (= bound-multiple 0) ratio
                              (>= bound-multiple 0) (* bound-multiple 2)
                              :else (/ ratio (* 2 bound-multiple)))
        ]
    (user/spy
     {:scale (map :set (hx :scale))
      ;; node-index
      :next-index next-node-index
      :next (:set ((vec (hx :scale)) next-node-index))
      :transposition bound-multiple*
      :dir direction})
    #_(interval ref-node intval))

(defn filter-scale
  "Get a subscale that only has degrees related to the `generators`"
  [scale generators]
  (filter #(-> % :set (set/intersection generators) not-empty)
          scale))
(comment
  (require '[clojure.test :refer [deftest testing is run-tests]])

  (deftest filter-scale-test
    (let [hex (->> [1 3 5 7] (->set 2) set->maps (bound-ratio 2)
                   (maps->data :bounded-ratio) :scale) ]
      (is (= #{#{7 1} #{1 5} #{1 3}}
             (->> (filter-scale hex #{1}) (map :set) set)))

      (is (= #{#{7 1} #{1 5} #{1 3} #{3 5} #{3 7}}
             (->> (filter-scale hex #{1 3}) (map :set) set)))))
  (run-tests))

(comment

  ;;interval-melody::hexany initial-degree intervals
  (interval-melody  hx 0 [0 2 1 -5 1 2 -1 2])
  ;;melody:: hexany degrees
  (melody hx [0 0 1 5 4 11])
  (hx))
(->>
 (->set 2 [1 3 5 7 9 11])
 (sort-by #(apply * %))
 (map sort)
 )


(comment
;;;
  (->> [1 3 5 7]
       #_(map #(/ % 7))
       (->set 2)
       (map #(apply * %))
       (map #(* % 4/5))
       #_set->maps
       (map #(within-bounding-period 2 %))
       sort
       #_(map ratio->cents)
       #_(map #(- % 0.09375))
       #_(sort-by #(-> % :bounded-ratio))
       #_(map (comp  :bounded-ratio))
       #_((fn [pairs] ;;; Group pair by contained generators
            (let [gens (set (apply concat pairs))]
              (reduce (fn [acc g]
                        (assoc acc g (filter #(% g) pairs))
                        ) {} gens))))
       user/spy)
  (->> [1 3 5 7 9 11]
       (->set 3)
       set->maps
       (bound-ratio 2)
       (sort-by #(-> % :bounded-ratio))
       (map (comp #_sort :set))
       #_((fn [pairs] ;;; Group pair by contained generators
            (let [gens (set (apply concat pairs))]
              (reduce (fn [acc g]
                        (assoc acc g (filter #(% g) pairs))
                        ) {} gens))))
       user/spy)

  (->> [1 3 5 7 9 11]
       (->set 3)
       set->maps
       (bound-ratio 2)
       (sort-by #(-> % :bounded-ratio))
       (map #(assoc % :cents (ratio->cents (:bounded-ratio %))))
       (mapv :cents)
       #_(#(select-keys % [0 3 5 10 18]))
       #_ #_ vals vec
       #_(map (comp #_sort :set))
       #_((fn [pairs] ;;; Group pair by contained generators
            (let [gens (set (apply concat pairs))]
              (reduce (fn [acc g]
                        (assoc acc g (filter #(% g) pairs))
                        ) {} gens))))
       user/spy)

  (->> [ 3 5 7 11]
       (->set 2)
       set->maps
       (bound-ratio 2)
       (sort-by #(-> % :bounded-ratio))
       (map :bounded-ratio)
       ;; get ascending intervals ordered by smallest
       (->set 2)
       (mapcat #(->> % sort reverse
                     (apply /)
                     (hash-map %)))
       (map second)
       (map ratio->cents)
       sort
       user/spy)

  (->> [1 3 5 7]
       (->set 2)
       set->maps
       (bound-ratio 2)
       (map :set)
       (sort-by (partial apply *))
       #_((fn [pairs] ;;; Group pair by contained generators
            (let [gens (set (apply concat pairs))]
              (reduce (fn [acc g]
                        (assoc acc g (filter #(% g) pairs))
                        ) {} gens))))
       user/spy))





(select-keys [0 10 20 30 40] [0 1])
