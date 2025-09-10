(ns erv.meru.recurrent-series)

(defn seq-ratios* [recurrent-seq]
  (->> recurrent-seq
       (partition 2 1)
       (map (fn [[a b]] (/ b a)))))

(defn seq-ratios [recurrent-seq]
  (->> recurrent-seq
       (partition 2 1)
       (map (fn [[a b]] (double (/ b a))))))

(declare converges-at)

(defn converges-at
  "Returns the index at which the recurrent-seq converges."
  [recurrent-seq & {:keys [ignore-first]
                    :or {ignore-first 0}}]
  (->> recurrent-seq
       (drop ignore-first)
       seq-ratios
       (partition 5 1)
       (take-while (fn [ns] (apply not= ns)))
       count
       (+ ignore-first)))

(def scale-formulas
  {:fibonacci {:i1 1 :i2 2 :f +}
   :meta-pelog {:i1 1 :i2 3 :f +}
   :meta-slendro {:i1 2 :i2 3 :f +}})

(defn recurrent-series
  "Creates a recurrent integer sequence and some data associated to it.
  Config:
      `:seed` A sequence of intergers to start the recurrent sequence.
      `:formula` A keyword that should be contained in `scale-formulas`. It automatically provides the arguments below, so can be used in place of these.
    In case no `:formula` is used:
      `:i1` The lowest index in the formula. - If this is confusing, read below.
      `:i2` The next index in the formula.
      `:f` The function to apply to these indexes (probably always, it will be +)


  For example on page 40 of https://anaphoria.com/merufour.pdf there is the Meta-Slendro formula:
  Hn-3 + Hn-2 = Hn
  `:i1` corresponds to 2, taken from Hn-2
  `:i2` corresponds to 3, taken from Hn-3."
  [{:keys [seed formula _i1 _i2 _f] :as config}]
  (let [config* (get scale-formulas formula config)
        {:keys [i1 i2 f] :or {f +}} config*
        seed*  (mapv #?(:clj bigint :cljs js/BigInt) seed)
        _ (when (> i2 (count seed))
            (throw (ex-info "The `seed` size must be equal or greater than `i1`" config*)))
        _ (when (>= i1 i2)
            (throw (ex-info "`i2` must be greater than `i1`" config*)))
        series (loop [seq* seed*
                      a (first (take-last i1 seed))
                      b (first (take-last i2 seed))]
                 (let [seq** (conj seq* (f a b))
                       a* (first (take-last i1 seq**))
                       b* (first (take-last i2 seq**))]
                   (if (apply = (seq-ratios (take-last 6 seq**)))
                     seq**
                     (recur seq** a* b*))))]
    {:convergence-double (last (seq-ratios series))
     :convergence (last (seq-ratios* series))
     :convergence-index (converges-at series)
     :series series}))
