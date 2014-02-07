(defproject com.viasat/arrows "1.1.7"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.clojure/monads "1.0.1"]]
  :description "A protocol for implementing arrows"
  :repositories ~(some #(try (load-file (str % "repositories.clj")) (catch Exception e)) ["../../" ""]))
