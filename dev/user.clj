(ns user
  (:require
   #_[overtone.core :as o :refer :all :exclude [tap]]
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh refresh-all]]
   [potemkin :refer [import-vars]]
   [clj-utils.core]
   [clojure.string :as str]))

(set-refresh-dirs "test" "src")
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


(defn connect []
  (eval
   '(do (require '[overtone.core :as o :refer :all :exclude [tap]])
        (let [windows? (clojure.string/includes? (System/getProperty "os.name")
                                      "Windows")]
          (cond
            (or (o/server-connected?) (o/server-connecting?)) :already-connected
            windows? (o/connect-external-server)
            :else (o/boot-external-server))))))

(defn test-sound []
  (eval '(demo (sin-osc 400))))
