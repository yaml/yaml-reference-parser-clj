(ns yaml-parser.core
  "Public API for the YAML reference parser."
  (:require [yaml-parser.grammar]
            [yaml-parser.parser :as parser]
            [yaml-parser.receiver :as receiver]))

(defn parse
  "Parse a YAML string and return a vector of YAML event maps."
  [yaml]
  (let [receiver (receiver/make-receiver-with-callbacks)
        parser (parser/make-parser receiver)]
    (parser/parse parser (or yaml ""))
    @(:events receiver)))

(def parse-yaml
  "Alias for parse, kept for compatibility with the original Clojure port."
  parse)
