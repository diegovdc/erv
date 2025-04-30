(ns user
  (:require
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh]]))

(set-refresh-dirs "test" "src")

(defn reset [] (refresh))

