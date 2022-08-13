(ns user
  (:require
   [overtone.core :as o :refer :all :exclude [tap]]
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh refresh-all]]
   #_[potemkin :refer [import-vars]]
   #_[clj-utils.core]
   #_[clojure.string :as str]))

(set-refresh-dirs "test" "src")
(defn reset [] (refresh))

#_(import-vars
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

#_(defn connect []
    (eval
     '(do (require '[overtone.core :as o :refer :all :exclude [tap]])
          (let [windows? (clojure.string/includes? (System/getProperty "os.name")
                                                   "Windows")]
            (cond
              (or (o/server-connected?) (o/server-connecting?)) :already-connected
              windows? (o/connect-external-server)
              :else (o/boot-external-server))))))

#_(defn test-sound []
    (eval '(demo (sin-osc 400))))

