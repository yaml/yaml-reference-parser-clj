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

(deftest memoizes-nested-flow-sequences
  (testing "nested flow sequence"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "sequence_start" :flow true}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a"}
            {:event "sequence_end"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "[[a]]\n")))))

(deftest replays-anchor-tag-alias-events
  (testing "anchors, aliases and tags inside a flow sequence"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a" :anchor "x"}
            {:event "alias" :name "x"}
            {:event "scalar" :value "b" :tag "tag:yaml.org,2002:str"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "[&x a, *x, !!str b]\n")))))

(deftest parses-flow-sequence-as-mapping-key
  (testing "flow sequence as an implicit block mapping key"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "mapping_start" :flow false}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a"}
            {:event "sequence_end"}
            {:event "scalar" :value "b"}
            {:event "mapping_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "[a]: b\n")))))

(deftest parses-multiple-documents-with-flow-content
  (testing "two explicit documents each holding a flow sequence"
    (is (= [{:event "stream_start"}
            {:event "document_start" :explicit true}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "document_start" :explicit true}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "b"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "--- [a]\n--- [b]\n")))))

(deftest folds-multiline-flow-scalar
  (testing "a plain scalar folded across lines inside a flow sequence"
    (is (= [{:event "stream_start"}
            {:event "document_start"}
            {:event "sequence_start" :flow true}
            {:event "scalar" :value "a b"}
            {:event "sequence_end"}
            {:event "document_end"}
            {:event "stream_end"}]
           (yaml-parser/parse "[a\n b]\n")))))

(deftest parses-deeply-nested-flow-in-linear-time
  (testing "20-deep nested flow sequence (exponential without memoization)"
    (let [depth 20
          yaml (str (apply str (repeat depth "["))
                    (apply str (repeat depth "]"))
                    "\n")
          start (System/nanoTime)
          events (yaml-parser/parse yaml)
          secs (/ (- (System/nanoTime) start) 1e9)]
      (is (= (+ 4 (* 2 depth)) (count events)))
      (is (= depth (count (filter #(= "sequence_start" (:event %)) events))))
      (is (= depth (count (filter #(= "sequence_end" (:event %)) events))))
      ;; Generous bound: unmemoized this is 2^20 work (minutes)
      (is (< secs 10)))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'yaml-parser.core-test)]
    (System/exit (+ fail error))))
