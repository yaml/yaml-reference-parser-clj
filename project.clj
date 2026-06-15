(defproject org.yamlstar/yaml-parser "0.1.0-SNAPSHOT"
  :description "Pure Clojure YAML 1.2 reference parser"
  :url "https://github.com/yaml/yaml-reference-parser-clj"
  :license {:name "MIT"
            :url "https://opensource.org/license/mit/"}
  :scm {:name "git"
        :url "https://github.com/yaml/yaml-reference-parser-clj"}
  :dependencies [[org.clojure/clojure "1.12.0"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :deploy-repositories [["releases" {:url "https://repo.clojars.org"
                                     :username :env/clojars_username
                                     :password :env/clojars_password
                                     :sign-releases false}]])
