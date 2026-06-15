(ns tap
  "TAP (Test Anything Protocol) output library for Babashka tests"
  (:require [clojure.string :as str]))

;; TAP output functions
(defn plan
  "Output TAP plan line"
  [n]
  (println (str "1.." n)))

(defn ok
  "Output TAP ok line"
  [n description]
  (println (str "ok " n " - " description)))

(defn not-ok
  "Output TAP not ok line with optional diagnostics"
  ([n description]
   (not-ok n description nil))
  ([n description diagnostics]
   (println (str "not ok " n " - " description))
   (when diagnostics
     (doseq [line (if (string? diagnostics)
                    [diagnostics]
                    diagnostics)]
       (println (str "  # " line))))))

(defn diag
  "Output TAP diagnostic line(s)"
  [& lines]
  (doseq [line lines]
    (println (str "# " line))))

(defn bail-out
  "Output TAP bail out"
  ([] (println "Bail out!"))
  ([reason] (println (str "Bail out! " reason))))

;; Test result tracking
(defn make-runner
  "Create a test runner state"
  []
  {:test-num (atom 0)
   :passed (atom 0)
   :failed (atom 0)})

(defn run-test
  "Run a single test, updating runner state and outputting TAP"
  [runner pass? description & [diagnostics]]
  (let [n (swap! (:test-num runner) inc)]
    (if pass?
      (do
        (swap! (:passed runner) inc)
        (ok n description))
      (do
        (swap! (:failed runner) inc)
        (not-ok n description diagnostics)))
    pass?))

(defn summary
  "Output test summary and return exit code"
  [runner]
  (let [passed @(:passed runner)
        failed @(:failed runner)
        total (+ passed failed)]
    (diag (str "Passed: " passed "/" total))
    (diag (str "Failed: " failed "/" total))
    (if (zero? failed) 0 1)))

;; Comparison helpers
(defn diff-lines
  "Generate diff diagnostics for mismatched strings"
  [expected actual]
  (let [exp-lines (when expected (str/split-lines expected))
        act-lines (when actual (str/split-lines actual))]
    (concat
     ["Expected:"]
     (map #(str "  " %) (or exp-lines ["(nil)"]))
     ["Actual:"]
     (map #(str "  " %) (or act-lines ["(nil)"])))))
