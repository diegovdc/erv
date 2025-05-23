(ns erv.cps.core
  ;;  TODO use https://github.com/Engelberg/ubergraph for the graphs
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [erv.utils.core :refer [interval validate]]
   #? (:cljs [goog.string :as gstr])
   #? (:cljs [goog.string.format])))

#?(:cljs
   (def format gstr/format))

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

(defn- num->kw [n]
  (-> (char (+ 65 n)) str str/lower-case keyword))

(defn ->cps [size factors]
  (if (> size (count factors))
    #{#{}}
    (with-meta
      (->> (combo/combinations (into [] factors) size)
           (map set)
           set)
      {::type (str size ")" (count factors))
       ::archi-factors (->> factors
                            sort
                            (map-indexed (fn [i fac] {fac (num->kw i)}))
                            (into {}))})))

(comment
  ;; TODO, for stellating the hexany

  (set/union (combo/combinations [1 2 3 4 1 2 3 4] 2)
             (let [gens [1 2 3 4]]
               (->> (combo/combinations gens 3)
                    (map (fn [c] (let [diff (set/difference (set gens) (set c))]
                                   (set/union c (set (map #(/ 1 %) diff))))))
                    (map set)
                    set))))
(defn set->maps
  "Creates a hexany-map"
  [cps-set]
  (let [archi-factors (::archi-factors (meta cps-set) (fn [_]))]
    (map (fn [pair] {:set pair
                     :archi-set (set (map archi-factors pair))}) cps-set)))

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
  i.e. for a cps with factors [1 3 5 7] you can use either 3, 5, or 7"
  ([bounding-period cps-map] (bound-ratio bounding-period 1 cps-map))
  ([bounding-period normalization-generator cps-map]
   (->> cps-map
        (map (fn [node*]
               (let [ratio (/ (apply * (node* :set)) normalization-generator)]
                 (if bounding-period;; TODO test `nil` `bounding-period` with sub-cps
                   (assoc node*
                          :ratio ratio
                          :bounded-ratio (within-bounding-period
                                          bounding-period
                                          ratio)
                          :bounding-period bounding-period)
                   (assoc node*
                          :ratio ratio
                          :bounded-ratio ratio
                          :bounding-period nil))))))))

(defn add-edge
  ;;  TODO use https://github.com/Engelberg/ubergraph for the graphs
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
                       (filter #(= (dec (count (:set %))) (count (set/intersection (:set current-node)
                                                                                   (:set %)))))
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

(declare get-cps-description)
(defn +meta [size factors norm-fac data
             & {:keys [type] :or {type (str size ")" (count factors))}}]
  (merge {:meta {:scale :cps
                 :period (-> data :scale first :bounding-period)
                 :size (-> data :scale count)
                 :cps/size size
                 :cps/factors (sort (into [] factors))
                 :cps/normalized-by norm-fac
                 :cps/type type}}
         data))

(defn filter-scale
  "Get a subscale that only has degrees related to the `factors`.
  `factors` must be a set."
  [scale factors]
  (filter #(-> % :set (set/intersection factors) not-empty)
          scale))

(defn find-subcps
  [cps-set-size factors sub-cps-set-size subcps-factors-size]
  (let [base-cps (->cps subcps-factors-size factors)
        diff-set-size (Math/abs (- cps-set-size sub-cps-set-size))
        gens-set (set factors)
        meta* {::type (str sub-cps-set-size ")" subcps-factors-size " of " cps-set-size ")" (count factors))}]
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
(comment
  (find-subcps 3 [1 3 5 7 11 13] 2 4))

(comment
  (find-subcps 3 [:a :b :c :d :e :f] 2 5)
  ;; For test
  (= (find-subcps 2 [:a :b :c :d :e] 2 4)
     #{#{#{:e :a} #{:b :a} #{:c :a} #{:e :c} #{:e :b} #{:c :b}}
       #{#{:e :a} #{:b :d} #{:b :a} #{:e :b} #{:e :d} #{:d :a}}
       #{#{:c :d} #{:b :d} #{:e :c} #{:e :b} #{:e :d} #{:c :b}}
       #{#{:c :d} #{:e :a} #{:c :a} #{:e :c} #{:e :d} #{:d :a}}
       #{#{:c :d} #{:b :d} #{:b :a} #{:c :a} #{:d :a} #{:c :b}}})

  (-> (find-subcps 2 [:a :b :c :d :e] 2 4) first meta))

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

(defn- factors->str [factors]
  (->> factors sort (str/join ".")))

(defn get-cps-description [cps]
  {:pre [(validate ::cps cps)]}
  (let [constants (apply set/intersection cps)
        non-constants (set/difference (apply set/union cps) constants)]
    (str/trim
     (str (::type (meta cps)) " "
          (cond
            (and (not-empty constants) (not-empty non-constants))
            (str (factors->str constants) "-" (factors->str non-constants))
            (not-empty constants) (factors->str constants)
            (not-empty non-constants) (factors->str non-constants)
            :else "")))))

(comment (get-cps-description (->cps 2 [1 3 5 7])))

(defn subcps-sets->map [subcps-set]
  (->>  subcps-set
        (map (juxt get-cps-description identity))
        (into {})))

(defn subcps-sets->data [period norm-fac subcps-set]
  (->>  subcps-set
        (map (juxt get-cps-description
                   #(->> %
                         set->maps
                         (bound-ratio period norm-fac)
                         (maps->data :bounded-ratio)
                         ((fn [data] (+meta (-> % first count)
                                            (set (apply concat %))
                                            norm-fac
                                            data
                                            :type (::type (meta %))))))))
        (into {})))

(comment
  (->> (find-subcps 4 [:a :b :c :d] 2 3)))

(defn filter-subcps-map
  ([subcps-map factors] (filter-subcps-map subcps-map factors false))
  ([subcps-map factors match-all?]
   (if (empty? factors)
     subcps-map
     (let [match-fn (if match-all? every? some)
           search-fns (map #(fn [k] (str/includes? k (str %))) factors)
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
  [size factors & {:keys [period norm-fac] :or {period 2 norm-fac 1}}]
  (->> factors
       (->cps size)
       set->maps
       (bound-ratio period norm-fac)
       (maps->data :bounded-ratio)
       (+meta size factors norm-fac)))

(defn +subcps [cps-data set-size factors-size]
  (let [{:keys [cps/size cps/factors period cps/normalized-by]} (:meta cps-data)]
    (update cps-data :subcps merge
            (->> (find-subcps size factors set-size factors-size)
                 (subcps-sets->data period normalized-by)))))

(comment (+subcps (make 2 [1 3 5 7] :period 2) 2 3))

(defn get-possible-subcps-data
  "Get the cell-row data for all possible subcps -i.e an inverted pascal's traingle. Note the `cps-size` is the cell in the triangle

  Returns [[row [cps size]]]"
  [cps-size cps-row]
  (->> (range 1 cps-row)
       (map (fn [row]
              [row (range (max 1 (+ cps-size (- row cps-row)))
                          (inc (min row cps-size)))]))))
(defn +all-subcps-row [cps-data [row cps-sizes]]
  (reduce (fn [cps-data* set-size]
            (+subcps cps-data* set-size row))
          cps-data
          cps-sizes))

(defn +all-subcps [cps-data]
  (let [{:keys [cps/size cps/factors]} (:meta cps-data)]
    (->> (get-possible-subcps-data size (count factors))
         (reduce #(+all-subcps-row %1 %2) cps-data))))

(defn archi-subcps-sets
  "Archetypal subsets"
  [cps-set-size cps-factors-size]
  (mapcat (fn [[factors-size set-sizes]]
            (mapcat (fn [set-size]
                      (let [factors (map num->kw (range cps-factors-size))]
                        (map (fn [set*]
                               {:name (str/replace (get-cps-description set*) #":" "")
                                #_(format "%s)%s %s"
                                          set-size
                                          factors-size
                                          (->> set*
                                               (apply set/union)
                                               sort
                                               (map name)
                                               (str/join ".")))
                                :set set*})
                             (find-subcps cps-set-size
                                          factors
                                          set-size
                                          factors-size))))
                    set-sizes))
          (get-possible-subcps-data cps-set-size cps-factors-size)))

(comment
  (archi-subcps-sets 3 6))

(defn get-subcps-basic-data [cps-data]
  (let [parse-type-as-numbers
        (fn [type*]
          (-> type* first
              (str/split #" ")
              first (str/split #"\)")
              (->> (map #?(:clj read-string
                           :cljs js/Number)))))]
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

(defn intersect
  "Get the scale that intersects two cps"
  [cps-1 cps-2]
  (let [set* (->> cps-2 :scale (map :set) set)]
    (->> cps-1 :scale (filter (fn [note] (set* (note :set)))))))

(comment
  (make 3 [1 3 5 7 11 13])
  (-> (make 3 [1 3 5 7 11 13])
      (+subcps 3 4)
      :subcps
      count)

  ;;  nested subcps

  (-> (make 2 [1 3 5 7 9 11])
      (+subcps 2 5)
      (update-in [:subcps "1.3.5.7.9"] +subcps 2 4)
      :subcps
      (get "1.3.5.7.9")
      :subcps
      (get "1.3.5.7")))

(comment
  (count (combo/combinations (range 6) 6))
  (->> (make 3 [6 7 8 9 10 11] :norm-fac (* 6 7 8) :period nil)
       :scale
       (map :ratio)
       sort)
  (sort  [1
          98
          5/4
          9/7
          11/8
          10/7
          3/2
          11/7
          45/28
          5/3
          99/56
          11/6
          15/8
          55/28
          33/16
          15/7
          55/24
          33/14
          55/21
          165/56]))
(comment
  (->> (make 3 [1 3 5 7 9 11])
       (+all-subcps)
       :subcps
       (sort-by first)
       reverse
       (map (juxt first (comp :cps/factors :meta second))))

  (->> (make 3 [1 3 5 7 9 11])
       :graphs :simple)

  (require
   '[clojure.test :refer [deftest testing is run-tests]]
   '[erv.utils.conversions :as conv]
   '[erv.scale.core :as scale])
  (scale/print-scale-intervals! (:nodes (make 2 [1 3 5 7]))
                                :unit :ratios
                                :ratio-type :ratio))
