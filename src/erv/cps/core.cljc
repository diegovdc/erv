(ns erv.cps.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [erv.utils.core :refer [validate]]))

(comment
  ;; data prototype
  ;; How to account for rotations, i.e. dividing the whole cps by 7 or 3 or 5,
  ;; that is by creating a 1:1 for the cps
  ;; How to add and calculate the hexanies, dekanies, etc of higher CPSs?
  {[1 3 5 7]
   {:scales {2 :some-scale
             3/2 :another-scale}
    :graph :some-graph}})

(s/def ::cps (s/and (s/coll-of set? :distinct true)
                    #(->> % (map count) (apply =))))
(s/def ::sub-cps-set (s/coll-of ::cps :distinct true))

(defn ->cps [size generators]
  (if (> size (count generators))
    #{#{}}
    (->> (combo/combinations (into [] generators) size)
         (map set)
         set)))

(defn set->maps
  "Creates a hexany-map"
  [hexany-set]
  (->> hexany-set
       (map (fn [pair] {:set pair}))))

(defn within-bounding-period
  "Transposes a ratio withing a bounding-period.
  The octave is a `bounding-period` of 2,the tritave of 3, etc."
  [bounding-period ratio]
  {:pre [(> bounding-period 1)]}
  (loop [r ratio]
    (cond
      (> r bounding-period) (recur (/ r bounding-period))
      (< r 1) (recur (* r bounding-period))
      (= bounding-period r) 1
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
  "Get a subscale that only has degrees related to the `generators`.
  `generators` must be a set."
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

(defn- generators->str [generators]
  (->> generators sort (str/join ".")))

(defn get-cps-description [cps]
  {:pre [(validate ::cps cps)]}
  (let [constants (apply set/intersection cps)
        non-constants (set/difference (apply set/union cps) constants)]
    (cond
      (and (not-empty constants) (not-empty non-constants))
      (str (generators->str constants) "-" (generators->str non-constants))
      (not-empty constants) (generators->str constants)
      (not-empty non-constants) (generators->str non-constants)
      :else "")))

(defn subcps-sets->map [subcps-set]
  (->>  subcps-set
        (map (juxt get-cps-description identity))
        (into {})))

(defn filter-subcps-map [subcps-map generators & [match-all?]]
  (if (empty? generators)
    subcps-map
    (let [match-fn (if match-all? every? some)
          search-fns (map #(fn [k] (str/includes? k (str %))) generators)
          filtered-keys (filter (comp (partial match-fn true?)
                                      (apply juxt search-fns))
                                (keys subcps-map))]
      (select-keys subcps-map filtered-keys))))

(comment (require '[user :refer [spy]]
                  '[clojure.test :refer [deftest testing is run-tests]]))
