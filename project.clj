(defproject erv "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [time-time "0.1.0-SNAPSHOT"]
                 [com.taoensso/timbre "4.10.0"]
                 [overtone "0.10.6"]
                 [table "0.5.0"]]
  :jvm-opts ["-Xmx8g"]
  :repl-options {:init-ns erv.core})
