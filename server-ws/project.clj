(defproject server-ws "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [http-kit "2.3.0-alpha2"]
                 [aleph "0.4.3"]
                 [compojure "1.6.0"]
                 [manifold "0.1.6"]
                 [org.clojure/core.async "0.3.443"]
                 [com.cognitect/transit-clj "0.8.300"]]
  :main server-ws.server)
