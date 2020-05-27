(ns erv.cps.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(comment
  ;; data prototype
  ;; How to account for rotations, i.e. dividing the whole cps by 7 or 3 or 5,
  ;; that is by creating a 1:1 for the cps
  ;; How to add and calculate the hexanies, dekanies, etc of higher CPSs?
  {[1 3 5 7]
   {:scales {2 :some-scale
             3/2 :another-scale}
    :graph :some-graph}})

(s/def ::cps (s/and (s/coll-of set? :distinct true) #(->> % (map count) (apply =))))
(s/def ::sub-cps-set (s/coll-of ::cps :distinct true))

(defn ->cps [size generators]
  (->> (combo/combinations (into [] generators) size) ;; returns all possible sets
       (remove (fn [set*] (when (seq set*)
                            (and (not= 1  (count set*))
                                 (apply = set*)))))
       (map set)
       set))

#_(->cps 1 #{1 2 3})
#_(->cps 2 [1 2 3])
#_(->cps 3 [1 2 3])

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


(defn filter-scale
  "Get a subscale that only has degrees related to the `generators`"
  [scale generators]
  (filter #(-> % :set (set/intersection generators) not-empty)
          scale))

(defn find-subcps
  [generators cps-set-size sub-cps-set-size subcps-generators-size]
  (let [base-cps (->cps subcps-generators-size generators)
        diff-set-size (Math/abs (- cps-set-size sub-cps-set-size))
        gens-set (set generators)]
    (->> base-cps (map
                   (fn [set*]
                     (let [diff (set/difference gens-set set*)
                           diffs (->cps diff-set-size diff)
                           hex (->cps sub-cps-set-size set*)]
                       (map (fn [d] (set (map #(set/union % d) hex))) diffs))))
         (apply concat)
         set)))




(comment (require '[user :refer [spy]]
                  '[clojure.test :refer [deftest testing is run-tests]]))
