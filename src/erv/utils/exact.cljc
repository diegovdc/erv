(ns erv.utils.exact
  "Parse numbers into ratios using `gfredericks/exact`. Also provides helpers for working around `exact` based numbers."
  (:require
   [clojure.math :refer [pow]]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [com.gfredericks.exact :as e]))

(defn- parseable-ratio?
  [s]
  (boolean (when (string? s)
             (re-matches #"^-?\d+(/-?\d+)?$" s))))

(defn parse-ratio
  [ratio-str]
  (when-not (parseable-ratio? ratio-str)
    (throw (ex-info "Cannot parse string into a ratio or an integer"
                    {:ratio-str ratio-str})))
  (let [[numer denom]
        (->> (str/split ratio-str #"/")
             (map (comp e/string->integer)))]
    (e// numer (or denom e/ONE))))

(defn ->exact
  "If on a `cljs` environment:
  Turn `x` into an `exact` integer or ratio.  If the value is already and instance of those,
  return the value as is."
  [x]
  #?(:clj x
     :cljs (cond
             (e/integer? x) x
             (e/ratio? x) x
             (int? x) (e/native->integer x)
             (parseable-ratio? x) (parse-ratio x)
             :else (throw (ex-info "Don't know how to turn value into `exact` instance"
                                   {:value x})))))

(defn exact-ratio->number
  [eratio]
  (/ (-> eratio
         e/numerator
         e/integer->native)
     (-> eratio
         e/denominator
         e/integer->native)))

#_(exact-ratio->number (e// (e/native->integer 2)
                            (e/native->integer 3)))
#_(number? (e// (e/native->integer 2)
                (e/native->integer 3)))
(defn ->native
  [x]
  (cond
    (number? x) x
    (e/integer? x) (e/integer->native x)
    (e/ratio? x) (exact-ratio->number x)
    :else (throw (ex-info "Don't know how to turn value into number"
                          {:value x}))))

(defn exact?
  [x]
  (or (e/integer? x) (e/ratio? x)))

(defn parse-ratios
  "Parses a string of ratios separated by `,` or whitespaces"
  [ratios-str]
  (->> (str/split ratios-str #"[,|\s]")
       (remove empty?)
       (map parse-ratio)))

#_(parse-scale "1   3/2\n 8/7")

(defn exact->string
  [exact-int-or-ratio]
  (cond
    (e/ratio? exact-int-or-ratio) (str
                                   (e/numerator exact-int-or-ratio)
                                   "/"
                                   (e/denominator exact-int-or-ratio))
    (e/integer? exact-int-or-ratio) (e/integer->string exact-int-or-ratio)
    :else (throw (ex-info "Don't know how to parse ratio"
                          {:value exact-int-or-ratio}))))

#_(map print-ratio (parse-scale "1   3/2\n 8/7"))

(defn make-readable
  "Takes a walkeable structure and converts all exact instances to a readable string"
  [coll]
  (walk/postwalk
   (fn [x]
     (if (or (e/integer? x) (e/ratio? x))
       (exact->string x)
       x))
   coll))

#?(:cljs
   (defn rationalize
     [num]
     (let [decimal-places (-> num str (str/split ".") last count)
           denom (int (pow 10 decimal-places))]
       (e// (e/native->integer (* denom num))
            (e/native->integer denom)))))
