(ns user
  (:require
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh refresh-all]]
   [potemkin :refer [import-vars]]
   [clj-utils.core]))

(set-refresh-dirs "src")
(defn reset [] (refresh))

(import-vars
 [clj-utils.core
  data
  clear-data
  spy
  spy->
  capture
  capture-all
  tap
  tap->
  tc
  tca])
