#!/usr/bin/env bb

;; YAML Test Suite Runner
;; Runs the official YAML test suite against our parser

(ns yaml-test-suite
  (:require [yaml-parser.parser :as p]
            [yaml-parser.test-receiver :as tr]
            [yaml-parser.grammar]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [babashka.fs :as fs]))

;; Load TAP library from test/lib
(load-file (str (fs/parent (System/getProperty "babashka.file")) "/lib/tap.clj"))
(require '[tap :as t])

;; Test suite directory
(def suite-dir
  (let [script-dir (-> (System/getProperty "babashka.file")
                       (fs/parent)
                       (fs/parent))]
    (str script-dir "/test/suite/src")))

;; Parse YAML and return events or nil on error
(defn parse-yaml [yaml-text]
  (let [receiver (tr/make-test-receiver)
        parser (p/make-parser receiver)]
    (try
      (p/parse parser yaml-text)
      (tr/output receiver)
      (catch Exception e
        nil))))

;; Normalize tree output for comparison
(defn normalize-tree [tree]
  (when tree
    (->> (str/split-lines tree)
         (map str/trim)
         (remove empty?)
         (str/join "\n"))))

;; Unescape special test characters
(defn unescape-yaml [text]
  (-> text
      (str/replace "\u2423" " ")           ; ␣ -> space
      (str/replace #"\u2014*\u00bb" "\t")  ; —» -> tab
      (str/replace "\u21d4" "\ufeff")      ; ⇔ -> BOM
      (str/replace "\u21b5" "")            ; ↵ -> nothing
      (str/replace #"\u220e\n?$" "")))     ; ∎ -> nothing

;; Load all tests from a file
;; Tests without :tree inherit from the previous test in the same file
;; Tests without :tree/:fail following a :skip test also inherit :skip
(defn load-tests [file]
  (let [content (slurp (str file))
        tests (yaml/parse-string content)]
    (loop [remaining tests
           last-tree nil
           last-skip nil
           result []]
      (if (empty? remaining)
        result
        (let [test (first remaining)
              ;; Use test's tree if present, otherwise inherit from last test
              tree (or (:tree test) last-tree)
              ;; Inherit skip status if test has no tree and no fail
              skip (or (:skip test)
                       (and (nil? (:tree test))
                            (nil? (:fail test))
                            last-skip))
              test-with-file (-> test
                                 (assoc :file (str (fs/file-name file)))
                                 ;; Only set inherited tree if test doesn't have fail: true
                                 (cond-> (and (nil? (:tree test))
                                              (not (:fail test))
                                              last-tree)
                                   (assoc :tree last-tree))
                                 ;; Inherit skip status
                                 (cond-> (and (nil? (:skip test)) skip)
                                   (assoc :skip skip)))]
          (recur (rest remaining)
                 tree
                 (or (:skip test) (and (nil? (:tree test)) last-skip))
                 (conj result test-with-file)))))))

;; Load all test files
(defn load-all-tests []
  (let [files (sort (fs/glob suite-dir "*.yaml"))]
    (mapcat load-tests files)))

;; Run a single test and return result map
(defn run-test [test]
  (let [name (:name test)
        file (:file test)
        yaml-text (unescape-yaml (or (:yaml test) ""))
        expected-tree (normalize-tree (:tree test))
        should-fail (:fail test)
        skip? (:skip test)
        result (parse-yaml yaml-text)
        actual-tree (normalize-tree result)
        description (str file " - " name)]
    (cond
      ;; Skip test - always passes
      skip?
      {:pass true :description description}

      ;; Expected to fail and did fail
      (and should-fail (nil? result))
      {:pass true :description description}

      ;; Expected to fail but succeeded
      should-fail
      {:pass false :description description
       :diagnostics ["Expected parse to fail but it succeeded"]}

      ;; No expected tree and no fail flag - treat as should fail
      (and (nil? expected-tree) (nil? result))
      {:pass true :description description}

      ;; No expected tree but parse succeeded - fail
      (and (nil? expected-tree) result)
      {:pass false :description description
       :diagnostics ["No expected tree but parse succeeded"
                     (str "Actual: " actual-tree)]}

      ;; Expected to succeed but failed
      (nil? result)
      {:pass false :description description
       :diagnostics ["Parse failed unexpectedly"]}

      ;; Compare trees
      (= actual-tree expected-tree)
      {:pass true :description description}

      ;; Trees don't match
      :else
      {:pass false :description description
       :diagnostics (t/diff-lines expected-tree actual-tree)})))

;; Main
(defn -main [& args]
  (let [tests (load-all-tests)
        runner (t/make-runner)]
    (t/plan (count tests))
    (doseq [test tests]
      (let [{:keys [pass description diagnostics]} (run-test test)]
        (t/run-test runner pass description diagnostics)))
    (System/exit (t/summary runner))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
