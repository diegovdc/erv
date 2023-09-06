(defproject erv "0.1.0-SNAPSHOT"
  :description "A library to design microtonal scales with ideas mainly derived from Erv Wilson's work"
  :url "https://github.com/diegovdc/erv"
  :license {:name "GPL-3.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html#license-text"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojars.videco/time-time "0.1.0-SNAPSHOT"]
                 [com.gfredericks/exact "0.1.11"]
                 [com.taoensso/timbre "4.10.0"]
                 [overtone "0.10.6"]
                 [table "0.5.0"]
                 [org.clojure/tools.namespace "1.3.0"]
                 [quil "4.0.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx8g"]
  :repl-options {:init-ns user})
