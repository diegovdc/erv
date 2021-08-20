(ns erv.cps.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [erv.utils.core :refer [interval validate]]))


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
    (with-meta
      (->> (combo/combinations (into [] generators) size)
           (map set)
           set)
      {::type (str size ")" (count generators))})))

(comment
  ;; TODO, for stellating the hexany

  (set/union (combo/combinations [1 2 3 4 1 2 3 4] 2)
             (let [gens [1 2 3 4]]
               (->> (combo/combinations gens 3)
                    (map (fn [c] (let [diff (set/difference (set gens) (set c))]
                                  (set/union c (map #(/ 1 %) diff)))))
                    (map set)
                    set))))

(defn set->maps
  "Creates a hexany-map"
  [cps-set]
  (map (fn [pair] {:set pair}) cps-set))

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
  "Calculate all the ratios within the bounding-period (e.g. octave, tritave, etc.)
  `normalization-generator` a generator used to normalize the scale so that it
  has a first root at 1/1.
  i.e. for a cps with generators [1 3 5 7] you can use either 3, 5, or 7"
  ([bounding-period cps-map] (bound-ratio bounding-period 1 cps-map ))
  ([bounding-period normalization-generator cps-map]
   (->> cps-map
        (map (fn [node*]
               (let [ratio (/ (apply * (node* :set)) normalization-generator)]
                 (assoc node*
                        :ratio ratio
                        :bounded-ratio (within-bounding-period
                                        bounding-period
                                        ratio)
                        :bounding-period bounding-period)))))))

(defn add-edge
  "Adds to the set of edges of `node-from` the value `node-to`"
  [graph node-from node-to]
  (update graph node-from (comp set conj) node-to))

(defn maps->graph
  "A map that connects each node with all the other nodes that share one element
  of the set"
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
  [scale-sort-fn cps-maps]
  (let [full-graph (maps->graph cps-maps)]
    {:scale (sort-by scale-sort-fn cps-maps)
     :nodes (sort-by :ratio cps-maps)
     :graphs {:full full-graph
              :simple (graph->simple-graph full-graph)}}))

(defn +meta [size generators norm-gen data
             & {:keys [type] :or {type (str size ")" (count generators))}}]
  (merge {:meta {:scale :cps
                 :size size
                 :generators (sort (into [] generators))
                 :period (-> data :scale first :bounding-period)
                 :normalized-by norm-gen
                 ::type type}}
         data))

(defn filter-scale
  "Get a subscale that only has degrees related to the `generators`.
  `generators` must be a set."
  [scale generators]
  (filter #(-> % :set (set/intersection generators) not-empty)
          scale))

(do
  ;; #dbg
  (defn find-subcps
    [cps-set-size generators sub-cps-set-size subcps-generators-size]
    (let [base-cps (->cps subcps-generators-size generators)
          diff-set-size (Math/abs (- cps-set-size sub-cps-set-size))
          gens-set (set generators)
          meta* {::type (str sub-cps-set-size ")" subcps-generators-size " of " cps-set-size ")" (count generators))}]
      (->> base-cps (map
                     (fn [set*]
                       (let [diff (set/difference gens-set set*)
                             diffs (->cps diff-set-size diff)
                             hex (->cps sub-cps-set-size set*)]
                         (map
                          (fn [d] (with-meta (set (map #(set/union % d) hex)) meta*))
                          diffs))))
           (apply concat)
           set)))
  (find-subcps 3 [1 3 5 7 11 13] 2 4))

(comment
  ;; For test
  (= (find-subcps 2 [:a :b :c :d :e] 2 4)
     #{#{#{:e :a} #{:b :a} #{:c :a} #{:e :c} #{:e :b} #{:c :b}}
       #{#{:e :a} #{:b :d} #{:b :a} #{:e :b} #{:e :d} #{:d :a}}
       #{#{:c :d} #{:b :d} #{:e :c} #{:e :b} #{:e :d} #{:c :b}}
       #{#{:c :d} #{:e :a} #{:c :a} #{:e :c} #{:e :d} #{:d :a}}
       #{#{:c :d} #{:b :d} #{:b :a} #{:c :a} #{:d :a} #{:c :b}}}))

(-> (find-subcps 2 [:a :b :c :d :e] 2 4) first meta)


(comment
  "If the common generator is factored out of the 3)5 dekany, the resulting hexany intersects with the corresponding 2)5 hexany"
  (defn mult-factors [vs] (let [factors (apply set/intersection vs)]
                            (if (seq factors) (apply * factors) 1)))
  (= (get (->> (find-subcps 2 [1 3 5 7 9] 2 4)
               subcps-sets->map
               (map (fn [[k vs]] [k (set (map #(/ (apply * %) (mult-factors vs)) vs))]))
               (into {}))
          "1.3.7.9")
     (get (->> (find-subcps 3 [1 3 5 7 9] 2 4)
               subcps-sets->map
               (map (fn [[k vs]] [k (set (map #(/ (apply * %) (mult-factors vs)) vs))]))
               (into {}))
          "5-1.3.7.9")))

(defn- generators->str [generators]
  (->> generators sort (str/join ".")))

(defn get-cps-description [cps]
  {:pre [(validate ::cps cps)]}
  (let [constants (apply set/intersection cps)
        non-constants (set/difference (apply set/union cps) constants)]
    (str (::type (meta cps)) " "
         (cond
           (and (not-empty constants) (not-empty non-constants))
           (str (generators->str constants) "-" (generators->str non-constants))
           (not-empty constants) (generators->str constants)
           (not-empty non-constants) (generators->str non-constants)
           :else ""))))

(get-cps-description (->cps 2 [1 3 5 7]))

(defn subcps-sets->map [subcps-set]
  (->>  subcps-set
        (map (juxt get-cps-description identity))
        (into {})))

(do
  (defn subcps-sets->data [period norm-gen subcps-set]
    (->>  subcps-set
          (map (juxt get-cps-description
                     #(->> %
                           set->maps
                           (bound-ratio period norm-gen)
                           (maps->data :bounded-ratio)
                           ((fn [data] (+meta (-> % first count)
                                             (set (apply concat %))
                                             norm-gen
                                             data
                                             :type (::type (meta %))))))))
          (into {})))

  (comment)
  (->> (find-subcps 4 [1 3 5 7 9 11] 2 4)
       (subcps-sets->data 2 1))
  )

(defn filter-subcps-map
  ([subcps-map generators] (filter-subcps-map subcps-map generators false))
  ([subcps-map generators match-all?]
   (if (empty? generators)
     subcps-map
     (let [match-fn (if match-all? every? some)
           search-fns (map #(fn [k] (str/includes? k (str %))) generators)
           filtered-keys (filter (comp (partial match-fn true?)
                                       (apply juxt search-fns))
                                 (keys subcps-map))]
       (select-keys subcps-map filtered-keys)))))

(defn sets-interval [set-a set-b] (/ (apply * set-b) (apply * set-a)))

(defn cps-intervals
  ([cps] (cps-intervals cps false))
  ([cps only-direct?]
   (->> (combo/combinations (into [] cps) 2)
        (reduce (fn [acc [set-a set-b]]
                  (if (and only-direct?
                           (empty? (set/intersection set-a set-b)))
                    acc
                    (-> acc
                        (update set-a assoc set-b (sets-interval set-a set-b))
                        (update set-b assoc set-a (sets-interval set-b set-a)))))
                {}))))

(defn cps-intervals-as-ratios
  ([cps] (cps-intervals-as-ratios cps false))
  ([cps only-direct?]
   (->> (cps-intervals cps only-direct?)
        (walk/postwalk #(if (set? %) (apply * %) %)))))

(defn denominator*
  "Trys to get the denominator for a number,
  if it's a ratio, returns the denominator,
  if the number is an integer, returns 1,
  if it is a float rationalizes and then returns the denominator."
  [n]
  #? (:clj (try (-> n rationalize denominator)
                (catch Exception e 1))
      :cljs 1))

(defn- group-intervals-by-denominator* [intervals-map]
  (->> intervals-map
       (group-by (comp denominator* val))
       (map (juxt key (comp (partial into {}) val)))
       (into {})))

(defn cps-intervals-by-denominator*
  "Takes the return value of `cps-intervals` and groups the intervals by their
  denominator"
  [cps-intervals]
  (->> cps-intervals
       (map (juxt key (comp group-intervals-by-denominator* val)))
       (into {})))

(defn cps-intervals-by-denominator
  "As`cps-intervals-by-denominator*` but composes `cps-intervals` so it only
  takes a `cps`"
  ([cps] (cps-intervals-by-denominator cps false))
  ([cps only-direct?]
   (-> cps (cps-intervals only-direct?)
       cps-intervals-by-denominator*)))

(defn set-chord
  "Returns a map of sets and intervals for a given set and optionally an interval
  (denominator)"
  ([cps-intervals-by-denominator set]
   (let [sets (-> cps-intervals-by-denominator
                  (get-in [set])
                  vals
                  (->> (apply merge)))]
     (if sets (assoc sets set 1) '())))
  ([cps-intervals-by-denominator set interval]
   (let [sets (-> cps-intervals-by-denominator
                  (get-in [set interval]))]
     (if sets (assoc sets set interval) '()))))

(defn make
  [size generators & {:keys [period norm-gen] :or {period 2 norm-gen 1}}]
  (->> generators
       (->cps size)
       set->maps
       (bound-ratio period norm-gen)
       (maps->data :bounded-ratio)
       (+meta size generators norm-gen)))


(defn +subcps [cps-data set-size generators-size]
  (let [{:keys [size generators period normalized-by]} (:meta cps-data)]
    (update cps-data :subcps merge
            (->> (find-subcps size generators set-size generators-size)
                 (subcps-sets->data period normalized-by)))))

(defn get-possible-subcps-data
  "Get the cell-row data for all possible subcps -i.e an inverted pascal's traingle. Note the `cps-size` is the cell in the triangle

  Returns [[row [cps size]]]"
  [cps-size cps-row]
  (->> (range 1 cps-row)
       (map (fn [row]
              [row (range (max 1 (+ cps-size (- row cps-row)))
                          (inc (min row cps-size)))]))))
(comment (get-possible-subcps-data 5 8))

(defn +all-subcps-row [cps-data [row cps-sizes]]
  (reduce (fn [cps-data* set-size]
            (+subcps cps-data* set-size row))
          cps-data
          cps-sizes))

(defn +all-subcps [cps-data]
  (let [{:keys [size generators]} (:meta cps-data)]
    (->> (get-possible-subcps-data size (count generators))
         (reduce #(+all-subcps-row %1 %2) cps-data))))

(defn get-subcps-basic-data [cps-data]
  (let [parse-type-as-numbers
        (fn [type*]
          (-> type* first
              (str/split #" ")
              first (str/split #"\)")
              (->> (map read-string))))]
    (->> cps-data
         :subcps
         (mapv (juxt first (comp #(map :set %) :scale  second)))
         (sort-by (juxt (comp second parse-type-as-numbers)
                        (comp first parse-type-as-numbers))))))
(comment
  (=
   (->> (make 2 [1 3 5 7])
        +all-subcps
        get-subcps-basic-data)
   [["1)1 of 2)4 1.5" '(#{1 5})]
    ["1)1 of 2)4 3.5" '(#{3 5})]
    ["1)1 of 2)4 3.7" '(#{7 3})]
    ["1)1 of 2)4 1.3" '(#{1 3})]
    ["1)1 of 2)4 5.7" '(#{7 5})]
    ["1)1 of 2)4 1.7" '(#{7 1})]
    ["1)2 of 2)4 7-1.5" '(#{7 5} #{7 1})]
    ["1)2 of 2)4 5-1.3" '(#{1 5} #{3 5})]
    ["1)2 of 2)4 7-1.3" '(#{7 3} #{7 1})]
    ["1)2 of 2)4 5-1.7" '(#{7 5} #{1 5})]
    ["1)2 of 2)4 3-1.5" '(#{1 3} #{3 5})]
    ["1)2 of 2)4 1-3.7" '(#{1 3} #{7 1})]
    ["1)2 of 2)4 5-3.7" '(#{7 5} #{3 5})]
    ["1)2 of 2)4 3-5.7" '(#{7 3} #{3 5})]
    ["1)2 of 2)4 1-5.7" '(#{1 5} #{7 1})]
    ["1)2 of 2)4 7-3.5" '(#{7 5} #{7 3})]
    ["1)2 of 2)4 3-1.7" '(#{7 3} #{1 3})]
    ["1)2 of 2)4 1-3.5" '(#{1 5} #{1 3})]
    ["2)2 of 2)4 3.5" '(#{3 5})]
    ["2)2 of 2)4 1.7" '(#{7 1})]
    ["2)2 of 2)4 1.3" '(#{1 3})]
    ["2)2 of 2)4 5.7" '(#{7 5})]
    ["2)2 of 2)4 3.7" '(#{7 3})]
    ["2)2 of 2)4 1.5" '(#{1 5})]
    ["1)3 of 2)4 1-3.5.7" '(#{1 5} #{1 3} #{7 1})]
    ["1)3 of 2)4 3-1.5.7" '(#{7 3} #{1 3} #{3 5})]
    ["1)3 of 2)4 7-1.3.5" '(#{7 5} #{7 3} #{7 1})]
    ["1)3 of 2)4 5-1.3.7" '(#{7 5} #{1 5} #{3 5})]
    ["2)3 of 2)4 1.3.5" '(#{1 5} #{1 3} #{3 5})]
    ["2)3 of 2)4 1.5.7" '(#{7 5} #{1 5} #{7 1})]
    ["2)3 of 2)4 1.3.7" '(#{7 3} #{1 3} #{7 1})]
    ["2)3 of 2)4 3.5.7" '(#{7 5} #{7 3} #{3 5})]]))




(comment
  (-> (make 3 [1 3 5 7 11 13])
      (+subcps 3 4)
      :subcps
      count)

  ;;  nested subcps

  (-> (make 2 [1 3 5 7 9 11 ])
      (+subcps 2 5)
      (update-in [:subcps "1.3.5.7.9"] +subcps 2 4)
      :subcps
      (get "1.3.5.7.9")
      :subcps
      (get "1.3.5.7")))


(comment
  (count (combo/combinations (range 6) 6)))
(comment
  (require
   '[user :refer [spy]]
   '[clojure.test :refer [deftest testing is run-tests]]
   '[erv.utils.conversions :as conv]
   '[erv.scale.core :as scale])
  (scale/print-scale-intervals! (:nodes (make 2 [3 5 7 11]))
                                :unit :ratios
                                :ratio-type :ratio))
