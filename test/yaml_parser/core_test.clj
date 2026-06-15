(ns yaml-parser.core-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [yaml-parser.core :as yaml-parser]))

(deftest parses-simple-mapping-events
  (testing "a simple block mapping"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "mapping_start" :flow false}
            {:event "scalar" :value "name"}
            {:event "scalar" :value "YAML"}
            {:event "mapping_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "name: YAML\n")))))

(deftest normalizes-missing-final-newline
  (testing "input without trailing newline"
    (is (= (yaml-parser/parse "name: YAML\n")
           (yaml-parser/parse "name: YAML")))))

(deftest parses-flow-sequence-events
  (testing "a flow sequence"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a"}
            {:event "scalar" :value "b"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "[a, b]\n")))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'yaml-parser.core-test)]
    (System/exit (+ fail error))))
