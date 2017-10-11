(defproject server-ws "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [aleph "0.4.3"]
                 [manifold "0.1.6"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/data.json "0.2.6"]
                 [com.cognitect/transit-clj "0.8.300"]]
  :main server-ws.server
  :source-paths ["src/clj" "andel/src/cljc" "noria_clj/src/clj"]
  :java-source-paths ["src/java"])
