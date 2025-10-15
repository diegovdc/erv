(ns erv.utils.parse
  "Parse numbers into ratios"
  (:require
   [clojure.string :as str]
   [com.gfredericks.exact :as e]))

(defn- parseable-ratio?
  [s]
  (boolean (re-matches #"^-?\d+(/-?\d+)?$" s)))

(do
  (defn parse-ratio
    [ratio-str]
    (when-not (parseable-ratio? ratio-str)
      (throw (ex-info "Cannot parse string into a ratio or an integer"
                      {:ratio-str ratio-str})))
    (let [[numer denom]
          (->> (str/split ratio-str #"/")
               (map e/string->integer))]
      (e// numer (or denom e/ONE))))

  #_(parse-ratio "3/2"))

(defn parse-scale
  "Parses a scale of ratio-strings"
  [scale-str]
  (->> (str/split scale-str #"[,|\s]")
       (remove empty?)
       (map parse-ratio)))

#_(parse-scale "1   3/2\n 8/7")

(defn print-ratio
  [exact-int-or-ratio]
  (cond
    (e/ratio? exact-int-or-ratio) (str
                                   (e/numerator exact-int-or-ratio)
                                   "/"
                                   (e/denominator exact-int-or-ratio))
    (e/integer? exact-int-or-ratio) (str
                                     (e/integer->string exact-int-or-ratio)
                                     "/"
                                     1)
    :else (throw (ex-info "Don't know how to parse ratio"
                          {:input exact-int-or-ratio}))))

#_(map print-ratio (parse-scale "1   3/2\n 8/7"))
