(ns yaml-parser.parser
  (:require [clojure.string :as str]
            [yaml-parser.prelude :refer :all]))

;; Forward declarations
(declare auto-detect auto-detect-indent trace-start trace-flush)

;; TRACE flag from environment
(def TRACE
  #?(:clj (Boolean/parseBoolean (or (env "TRACE") "false"))
     :glj (= (os.Getenv "TRACE") "true")))

;; Packrat memoization of flow-context rules. Disabled under TRACE
;; (memo hits skip subtree execution, which would change trace output)
;; and via YAML_PARSER_NO_MEMO=1 (A/B escape hatch).
(def MEMO
  #?(:clj (and (not TRACE) (nil? (env "YAML_PARSER_NO_MEMO")))
     :glj false))

;; Fast paths in call skip per-frame bookkeeping (state frame, receive
;; dispatch, memo gate) for calls whose dispatch node is nil, i.e.
;; where no receiver callback can fire. Disabled under TRACE (frames
;; feed trace output) and via YAML_PARSER_NO_FAST=1 (A/B escape hatch).
(def FAST
  #?(:clj (and (not TRACE) (nil? (env "YAML_PARSER_NO_FAST")))
     :glj false))

;; Rules safe for memoization: flow-context productions whose results
;; depend only on (pos, args, doc flag, receiver-cache depth) plus the
;; receiver volatiles snapshotted in memo-vol-keys. Their subtrees never
;; call set*/auto-detect or read state :m/:t (block-only machinery).
;; Must track rule names in the generated grammar (like callback-rules).
(def ^:private memo-rules
  #{"c_flow_sequence" "ns_s_flow_seq_entries" "ns_flow_seq_entry"
    "c_flow_mapping" "ns_s_flow_map_entries" "ns_flow_map_entry"
    "ns_flow_map_explicit_entry" "ns_flow_map_implicit_entry"
    "ns_flow_map_yaml_key_entry" "c_ns_flow_map_empty_key_entry"
    "c_ns_flow_map_json_key_entry" "c_ns_flow_map_separate_value"
    "c_ns_flow_map_adjacent_value"
    "ns_flow_pair" "ns_flow_pair_entry" "ns_flow_pair_yaml_key_entry"
    "c_ns_flow_pair_json_key_entry"
    "ns_s_implicit_yaml_key" "c_s_implicit_json_key"
    "ns_flow_node" "c_flow_json_node" "ns_flow_yaml_node"
    "ns_flow_content" "c_flow_json_content" "ns_flow_yaml_content"
    "ns_plain" "c_single_quoted" "c_double_quoted"
    ;; Separation/comment scans, re-parsed at the same position by
    ;; sibling alternatives in block context. s_separate_lines rather
    ;; than s_separate: the block/flow c variants all dispatch to
    ;; [s_separate_lines n], so one entry serves differing c.
    "s_separate_lines" "s_l_comments"
    "s_l_flow_in_block"})

;; Parse state frame. :node is the frame's precomputed receiver
;; dispatch node (see build-cb-roots/frame-node); :full marks subtrees
;; whose exact frame-stack shape is load-bearing (set* ancestor walks,
;; p/match scans), disabling the frameless fast path inside them.
(defrecord StateFrame [name node doc lvl beg end m t full])

;; Default state when stack is empty
(def default-state
  (->StateFrame nil nil false 0 0 nil nil nil false))

;; Parser state - uses volatiles for mutable state
(defn make-parser [receiver]
  (let [parser {:receiver (volatile! receiver)
                :input (volatile! "")
                :pos (volatile! 0)
                :end (volatile! 0)
                :state (volatile! [])
                :memo (volatile! {})
                :cb-roots (volatile! {})
                :cb-chains (volatile! #{})
                :trace-num (volatile! 0)
                :trace-line (volatile! 0)
                :trace-on (volatile! true)
                :trace-off (volatile! 0)
                :trace-info (volatile! ["" "" "" 0])}]
    ;; Link parser to receiver (stores parser directly in the receiver copy, not as atom)
    (vswap! (:receiver parser) assoc :parser parser)
    parser))

;; State management
(defn state-curr [parser]
  (let [state @(:state parser)]
    (if (empty? state)
      default-state
      (peek state))))

(defn state-prev [parser]
  (let [state @(:state parser)]
    (when (>= (count state) 2)
      (nth state (- (count state) 2)))))

;; Set of anchor rule names that have receiver callbacks.
;; Frames of rules outside this set carry a nil dispatch node, so
;; receive exits with a single map lookup for the ~85% of calls
;; where no callback can match.
(def ^:private callback-rules
  #{"l_yaml_stream" "ns_yaml_version" "c_tag_handle" "ns_tag_prefix"
    "c_directives_end" "c_document_end" "c_flow_mapping" "c_flow_sequence"
    "l_block_mapping" "l_block_sequence" "ns_l_compact_mapping"
    "ns_l_compact_sequence" "ns_flow_pair" "ns_l_block_map_implicit_entry"
    "c_l_block_map_explicit_entry" "c_ns_flow_map_empty_key_entry"
    "ns_plain" "c_single_quoted" "c_double_quoted" "l_empty"
    "l_nb_literal_text" "c_l_literal" "ns_char" "s_white"
    "s_nb_folded_text" "s_nb_spaced_text" "c_l_folded" "e_scalar"
    "s_l_block_collection" "c_ns_anchor_property" "c_ns_tag_property"
    "c_ns_alias_node"})

;; Frame-shape-sensitive subtrees. set* writes :m/:t into ancestor
;; frames positionally and stops at the s_l_block_scalar frame; p/match
;; scans for the deepest frame with :end set, at three grammar sites
;; (s_indent_lt, s_indent_le, c_indentation_indicator). Entering these
;; rules sets :full on the frame (inherited like :doc), which keeps
;; every descendant on the slow path so the stack shape those readers
;; see is unchanged.
(def ^:private full-frame-roots
  #{"s_l_block_scalar" "s_indent_lt" "s_indent_le"})

;; Block scalar CONTENT has no set*/match readers (they all live in the
;; header, fully parsed before content starts), so clear :full there;
;; the l_empty -> s_indent_lt/le descendants re-set it via the roots.
(def ^:private full-frame-clears
  #{"l_literal_content" "l_folded_content"})

;; Rules that must always get a real state frame even when their
;; dispatch node is nil: callback anchors keep receiver semantics,
;; memoized rules need the slow path's memo hook, l_bare_document
;; carries the :doc flag the-end reads, and the :full roots/clears
;; must exist to toggle the flag.
(def ^:private frame-required-rules
  (-> callback-rules
      (into memo-rules)
      (into full-frame-roots)
      (into full-frame-clears)
      (conj "l_bare_document")))

;; Receiver callback routing. Dispatch is resolved once per state
;; frame at push time: a frame whose rule name contains "_" is an
;; anchor and gets its node from cb-roots (nil unless the rule is in
;; callback-rules); a combinator frame extends its parent's node,
;; caching the resolved child per distinct chain in :kids. The chain
;; name matches the old scan-and-mangle scheme: anchor name joined
;; with the paren-stripped (or chr->hex) frame names above it.
;; :ext marks nodes whose chain is a proper prefix of some callback
;; key: real frames must exist below them so descendant chains resolve
;; exactly (both extension and the reset at rule frames). Below
;; non-:ext nodes every descendant chain is dead, so frames there are
;; pure bookkeeping and the frameless fast path may skip them.
(defn- build-cb-roots [receiver prefixes]
  (let [callbacks (:callbacks receiver)]
    (reduce (fn [acc name]
              (assoc acc name
                     {:chain name
                      :ext (contains? prefixes name)
                      :cbs {:try (get callbacks (str "try__" name))
                            :got (get callbacks (str "got__" name))
                            :not (get callbacks (str "not__" name))}
                      :kids (volatile! {})}))
            {}
            callback-rules)))

;; Set of every proper chain prefix of a callback key, e.g.
;; "got__c_flow_sequence__all__x5b" contributes "c_flow_sequence" and
;; "c_flow_sequence__all". A frame whose chain has no callbacks of its
;; own and is not on the way to one (not in this set) gets a nil node,
;; so receive exits immediately for the whole dead subtree.
(defn- callback-chain-prefixes [callbacks]
  (reduce (fn [acc k]
            (let [chain (str/replace k #"^(?:try|got|not)__" "")
                  parts (str/split chain #"__")]
              (loop [i 1 acc acc]
                (if (< i (count parts))
                  (recur (inc i) (conj acc (str/join "__" (take i parts))))
                  acc))))
          #{}
          (keys callbacks)))

(defn- frame-node [parser parent-node name]
  (if (str/includes? name "_")
    (get @(:cb-roots parser) name)
    (when parent-node
      (let [kids (:kids parent-node)
            k (get @kids name ::miss)]
        (if (identical? k ::miss)
          (let [mangled (if-let [[_ c] (re-matches #"chr\((.)\)" name)]
                          (str "x" (hex-char c))
                          (str/replace name #"\(.*" ""))
                chain (str (:chain parent-node) "__" mangled)
                callbacks (:callbacks @(:receiver parser))
                try-cb (get callbacks (str "try__" chain))
                got-cb (get callbacks (str "got__" chain))
                not-cb (get callbacks (str "not__" chain))
                ext (contains? @(:cb-chains parser) chain)
                node (when (or try-cb got-cb not-cb ext)
                       {:chain chain
                        :ext ext
                        :cbs {:try try-cb :got got-cb :not not-cb}
                        :kids (volatile! {})})]
            (vswap! kids assoc name node)
            node)
          k)))))

(defn state-push
  ([parser name]
   (let [curr (state-curr parser)]
     (state-push parser name false
                 (frame-node parser (:node curr) name))))
  ([parser name doc node]
   (let [curr (state-curr parser)]
     (vswap! (:state parser) conj
             (->StateFrame name
                           node
                           (if doc true (:doc curr))
                           (inc (:lvl curr))
                           @(:pos parser)
                           nil
                           (:m curr)
                           (:t curr)
                           (cond
                             (contains? full-frame-clears name) false
                             (contains? full-frame-roots name) true
                             :else (:full curr)))))))

(defn state-pop [parser]
  (let [state @(:state parser)
        child (peek state)
        parents (pop state)]
    (vreset! (:state parser)
             (if (seq parents)
               (let [curr (peek parents)]
                 (conj (pop parents)
                       (assoc curr
                              :beg (:beg child)
                              :end @(:pos parser))))
               parents))))

(defn receive [parser func type pos]
  (when-let [receiver-fn (get (:cbs (:node (state-curr parser))) type)]
    (let [curr-pos @(:pos parser)
          input @(:input parser)
          text (if (<= pos curr-pos)
                 (subs input pos curr-pos)
                 "")]
      (receiver-fn @(:receiver parser)
                   {:text text
                    :state (state-curr parser)
                    :start pos}))))

;; Memoization support. A memo record stores the parse outcome of a
;; whitelisted rule at a position: {:value :end-pos :events :entry-vols
;; :exit-vols}. Events are the net delta appended to the receiver frame
;; active at rule entry (the try/got/not cache brackets balance within
;; the call, so all committed events land in that frame). The receiver
;; volatiles below are the only mutable receiver state reachable from
;; flow-rule handlers; a hit is valid only when their current values
;; equal the recorded entry snapshot.
(def ^:private memo-vol-keys
  [:anchor :tag :tag-map :tag-handle
   :document-start :document-end :in-scalar :first])

(defn- memo-vols [receiver]
  (mapv #(deref (get receiver %)) memo-vol-keys))

(defn- memo-restore-vols! [receiver vols]
  (dorun (map (fn [k v] (vreset! (get receiver k) v))
              memo-vol-keys vols)))

(defn- memo-frame-len [receiver top?]
  (if top?
    (count @(:events receiver))
    (count (peek @(:cache receiver)))))

(defn- memo-delta [receiver top? base]
  (let [frame (if top?
                @(:events receiver)
                (peek @(:cache receiver)))]
    (subvec frame base)))

(defn- memo-replay! [parser receiver top? hit]
  (vreset! (:pos parser) (:end-pos hit))
  (when (seq (:events hit))
    (if top?
      (vswap! (:events receiver) into (:events hit))
      (vswap! (:cache receiver)
              (fn [c] (conj (pop c) (into (peek c) (:events hit)))))))
  (memo-restore-vols! receiver (:exit-vols hit))
  (:value hit))

;; Forward declarations for grammar functions
(declare call)

;; The central call mechanism
(defn call
  ([parser func] (call parser func "boolean"))
  ([parser func type]
   (let [fvec (when (vector? func) func)
         func (if fvec (nth fvec 0) func)]
     ;; If func is a number or string, return it directly
     (cond
       (number? func) func
       (string? func) func
       :else
       (do
         (when-not (fn? func)
           (FAIL (str "Bad call type '" (typeof* func) "' for '" func "'")))

         (let [fmeta #?(:clj (meta func) :glj nil)
               trace (or #?(:clj (:trace fmeta) :glj (func-name func))
                         (str func))
               curr (state-curr parser)
               node (frame-node parser (:node curr) trace)
               ;; Frameless additionally requires the parent node to be
               ;; non-extensible: a skipped frame's descendants resolve
               ;; their chains against the nearest real ancestor, so
               ;; skipping a frame under an :ext node would let inner
               ;; combinators of unrelated rules reconnect to a live
               ;; chain and fire its callbacks (rule frames reset
               ;; chains; that reset must be preserved by a real
               ;; frame). Leaves have no descendants, so their own nil
               ;; node suffices.
               fast (when (and FAST (nil? node))
                      (cond
                        (:leaf fmeta) :leaf
                        (and (not (:ext (:node curr)))
                             (not (:full curr))
                             (not (contains? frame-required-rules trace)))
                        :frameless
                        :else nil))]
           (case fast
             ;; Leaf fast path: chr/rng/chars matchers make no nested
             ;; calls and no callback can fire here, so skip the frame,
             ;; receive dispatch and memo gate. The parent frame still
             ;; gets the :beg/:end update state-pop would have written;
             ;; the p/match sites depend on it (safe even under :full).
             :leaf
             (let [pos0 @(:pos parser)
                   value (func parser)]
               (vswap! (:state parser)
                       (fn [s]
                         (let [i (dec (count s))]
                           (if (neg? i)
                             s
                             (assoc s i (assoc (nth s i)
                                               :beg pos0
                                               :end @(:pos parser)))))))
               value)

             ;; Frameless fast path: combinators and plain rules where
             ;; no callback can fire, outside frame-shape-sensitive
             ;; (:full) subtrees and not in frame-required-rules.
             ;; Everything they or their subtrees observe (:doc, :m,
             ;; :t via inheritance; pos via volatiles) is unaffected
             ;; by the missing frame.
             :frameless
             (let [args (when (and fvec (> (count fvec) 1))
                          (mapv (fn [a]
                                  (cond
                                    (vector? a) (call parser a "any")
                                    (fn? a) (call parser a "any")
                                    :else a))
                                (subvec fvec 1)))
                   value (loop [v (if (nil? args)
                                    (func parser)
                                    (case (count args)
                                      1 (func parser (nth args 0))
                                      2 (func parser (nth args 0)
                                              (nth args 1))
                                      (apply func parser args)))]
                           (if (or (fn? v) (vector? v))
                             (recur (call parser v))
                             v))]
               (when (and (not= type "any")
                          (if (= type "boolean")
                            (not (or (nil? value)
                                     (true? value)
                                     (false? value)))
                            (not= (typeof* value) type)))
                 (FAIL (str "Calling '" trace "' returned '" (typeof* value) "' instead of '" type "'")))
               value)

             ;; Slow path: full frame, receive dispatch, memoization
             (do
               (state-push parser trace (= trace "l_bare_document") node)

           ;; Evaluate arguments (skip mapv when no args)
           (let [args (when (and fvec (> (count fvec) 1))
                        (mapv (fn [a]
                                (cond
                                  (vector? a) (call parser a "any")
                                  (fn? a) (call parser a "any")
                                  :else a))
                              (subvec fvec 1)))
                 pos @(:pos parser)
                 receiver (when MEMO @(:receiver parser))
                 memo? (and MEMO
                            (= type "boolean")
                            (contains? memo-rules trace)
                            (nil? (some-> (:callback receiver) deref)))
                 top? (when memo? (empty? @(:cache receiver)))
                 memo-key (when memo?
                            [trace pos args top?
                             (boolean (:doc (state-curr parser)))])
                 entry-vols (when memo? (memo-vols receiver))
                 hit (when memo?
                       (let [h (get @(:memo parser) memo-key)]
                         (when (and h (= (:entry-vols h) entry-vols))
                           h)))
                 value
                 (if hit
                   (memo-replay! parser receiver top? hit)
                   (let [depth (when memo? (count @(:cache receiver)))
                         base (when memo? (memo-frame-len receiver top?))
                         _ (receive parser func :try pos)

                         ;; Call the function — bypass clojure.core/apply
                         ;; for the common arities to avoid lang.Apply
                         ;; []any allocation and reflection.
                         value (loop [v (if (nil? args)
                                          (func parser)
                                          (case (count args)
                                            1 (func parser (nth args 0))
                                            2 (func parser (nth args 0)
                                                    (nth args 1))
                                            (apply func parser args)))]
                                 (if (or (fn? v) (vector? v))
                                   (recur (call parser v))
                                   v))]

                     ;; Type checking - nil is treated as false for boolean type
                     (when (and (not= type "any")
                                (if (= type "boolean")
                                  (not (or (nil? value)
                                           (true? value)
                                           (false? value)))
                                  (not= (typeof* value) type)))
                       (FAIL (str "Calling '" trace "' returned '" (typeof* value) "' instead of '" type "'")))

                     ;; Handle result
                     (if (not= type "boolean")
                       nil
                       (if value
                         (receive parser func :got pos)
                         (receive parser func :not pos)))

                     ;; The cache-depth check guards against a rule that
                     ;; leaves the receiver frame stack unbalanced; such
                     ;; a result cannot be replayed from its entry frame.
                     (when (and memo?
                                (= depth (count @(:cache receiver))))
                       (vswap! (:memo parser) assoc memo-key
                               {:value value
                                :end-pos @(:pos parser)
                                :events (memo-delta receiver top? base)
                                :entry-vols entry-vols
                                :exit-vols (memo-vols receiver)}))
                     value))]

             (state-pop parser)
             value)))))))))
;; Special functions - internal versions
(defn start-of-line* [parser]
  (let [pos @(:pos parser)
        input @(:input parser)]
    (or (= pos 0)
        (>= pos @(:end parser))
        (= (nth input (dec pos)) \newline))))

(defn end-of-stream* [parser]
  (>= @(:pos parser) @(:end parser)))

#?(:clj
   (defn- doc-end-marker?
     "True when input at pos starts with --- or ... followed by
     whitespace or end of input. Equivalent to the regex
     ^(?:---|\\.\\.\\.)((?=\\s)|$) without the per-call pattern
     compile and full-suffix substring."
     [^String input pos]
     (let [len (.length input)]
       (and (<= (+ pos 3) len)
            (let [c (.charAt input pos)]
              (and (or (= c \-) (= c \.))
                   (= c (.charAt input (inc pos)))
                   (= c (.charAt input (+ pos 2)))))
            (or (= (+ pos 3) len)
                (let [c (.charAt input (+ pos 3))]
                  ;; Java \s is [ \t\n\x0B\f\r]
                  (or (= c \space) (= c \tab) (= c \newline)
                      (= c \return) (= c \formfeed)
                      (= c (char 11)))))))))

(defn the-end [parser]
  (or (end-of-stream* parser)
      (and (:doc (state-curr parser))
           (start-of-line* parser)
           #?(:clj (doc-end-marker? @(:input parser) @(:pos parser))
              :glj (let [remaining (subs @(:input parser) @(:pos parser))
                         prefix (re-find #"^(?:---|\.\.\.)" remaining)]
                     (when prefix
                       (let [after (subs remaining (count prefix))]
                         (or (empty? after)
                             (re-find #"^\s" after)))))))))

;; Grammar-callable versions (return functions). The wrappers carry no
;; per-call state, so each is built once and shared.
(def ^:private start-of-line-rule
  (name* "start_of_line"
    (fn [p] (start-of-line* p))
    "start_of_line"))

(defn start-of-line [parser]
  start-of-line-rule)

(def ^:private end-of-stream-rule
  (name* "end_of_stream"
    (fn [p] (end-of-stream* p))
    "end_of_stream"))

(defn end-of-stream [parser]
  end-of-stream-rule)

(def ^:private empty-rule*
  (name* "empty"
    (fn [p] true)
    "empty"))

(defn empty-rule [parser]
  empty-rule*)

;; Character matching primitives. Rule bodies reconstruct their
;; combinator trees on every invocation, so these matchers are cached
;; by their construction args (they don't capture the parser). The
;; caches are bounded by the distinct chars/ranges in the grammar.
;; Matchers carry :leaf metadata: they consume at most one codepoint,
;; make no nested calls, and are eligible for call's leaf fast path.
;; Public because the generated grammar marks fused class rules too.
(defn leaf* [f]
  #?(:clj (vary-meta f assoc :leaf true)
     :glj f))

(def ^:private chr-cache (volatile! {}))

(defn chr [parser char]
  (or (get @chr-cache char)
      (let [trace (str "chr(" (stringify char) ")")
            c (first char)
            f (leaf*
               (name* trace
                 (fn chr-fn [p]
                   (when-not (the-end p)
                     (when (= (nth @(:input p) @(:pos p)) c)
                       (vswap! (:pos p) inc)
                       true)))
                 trace))]
        (vswap! chr-cache assoc char f)
        f)))

(def ^:private rng-cache (volatile! {}))

(defn rng [parser low high]
  (or (get @rng-cache [low high])
      (let [trace (str "rng(" (stringify low) "," (stringify high) ")")
            lo #?(:clj (Character/codePointAt ^String low 0)
                  :glj (int (first low)))
            hi #?(:clj (Character/codePointAt ^String high 0)
                  :glj (int (first high)))
            f (leaf*
               (name* trace
                 (fn rng-fn [p]
                   (when-not (the-end p)
                     (let [input @(:input p)
                           pos @(:pos p)
                           cp #?(:clj (Character/codePointAt ^String input pos)
                                 :glj (int (nth input pos)))]
                       (when (and (>= cp lo) (<= cp hi))
                         (vswap! (:pos p) + #?(:clj (Character/charCount cp)
                                                :glj 1))
                         true))))
                 trace))]
        (vswap! rng-cache assoc [low high] f)
        f)))

;; Fused character-class matcher. ranges is a flat, sorted,
;; non-overlapping vector [lo0 hi0 lo1 hi1 ...] of inclusive codepoint
;; bounds, computed at grammar-generation time from the spec's
;; chr/rng/any/but class algebra (see util/generate-yaml-grammar).
;; Matches one codepoint with rng semantics (codePointAt + charCount
;; advance, astral-aware), guarded by the-end. Constructed once per
;; site by the generated rule templates, so no cache is needed. The
;; constant trace name has no underscore, so frame-node treats it as
;; a chain element; no callback chain contains "chars".
(defn chars [parser ranges]
  (let [n (count ranges)]
    (leaf*
     (name* "chars"
       (fn chars-fn [p]
         (when-not (the-end p)
           (let [input @(:input p)
                 pos @(:pos p)
                 cp #?(:clj (Character/codePointAt ^String input pos)
                       :glj (int (nth input pos)))]
             (loop [i 0]
               (when (< i n)
                 (if (< cp (nth ranges i))
                   nil
                   (if (<= cp (nth ranges (inc i)))
                     (do (vswap! (:pos p) +
                                 #?(:clj (Character/charCount cp)
                                    :glj 1))
                         true)
                     (recur (+ i 2)))))))))
       "chars"))))

;; Combinators
(defn all [parser & funcs]
  (name* "all"
    (fn all-fn [p]
      (let [pos @(:pos p)]
        (loop [fs funcs]
          (if (empty? fs)
            true
            (let [f (first fs)]
              (when-not f
                (FAIL "*** Missing function in all group:" funcs))
              (if-not (call p f)
                (do
                  (vreset! (:pos p) pos)
                  false)
                (recur (rest fs))))))))
    "all"))

(defn any [parser & funcs]
  (name* "any"
    (fn any-fn [p]
      (loop [fs funcs]
        (if (empty? fs)
          false
          (if (call p (first fs))
            true
            (recur (rest fs))))))
    "any"))

(defn may [parser func]
  (name* "may"
    (fn may-fn [p]
      (call p func)
      true)
    "may"))

(defn rep [parser min max func]
  (let [trace (if TRACE (str "rep(" min "," max ")") "rep")]
    (name* trace
      (fn rep-fn [p]
        (if (and max (< max 0))
          false
          (let [pos-start @(:pos p)]
            (loop [count 0
                   pos @(:pos p)]
              (if (and max (>= count max))
                (if (and (>= count min) (or (nil? max) (<= count max)))
                  true
                  (do
                    (vreset! (:pos p) pos-start)
                    false))
                (if-not (call p func)
                  (if (and (>= count min) (or (nil? max) (<= count max)))
                    true
                    (do
                      (vreset! (:pos p) pos-start)
                      false))
                  (if (= @(:pos p) pos)
                    (if (and (>= count min) (or (nil? max) (<= count max)))
                      true
                      (do
                        (vreset! (:pos p) pos-start)
                        false))
                    (recur (inc count) @(:pos p)))))))))
      trace)))

(defn rep2 [parser min max func]
  (let [trace (if TRACE (str "rep2(" min "," max ")") "rep2")]
    (name* trace
      (fn rep2-fn [p]
        (if (and max (< max 0))
          false
          (let [pos-start @(:pos p)]
            (loop [count 0
                   pos @(:pos p)]
              (if (and max (>= count max))
                (if (and (>= count min) (or (nil? max) (<= count max)))
                  true
                  (do
                    (vreset! (:pos p) pos-start)
                    false))
                (if-not (call p func)
                  (if (and (>= count min) (or (nil? max) (<= count max)))
                    true
                    (do
                      (vreset! (:pos p) pos-start)
                      false))
                  (if (= @(:pos p) pos)
                    (if (and (>= count min) (or (nil? max) (<= count max)))
                      true
                      (do
                        (vreset! (:pos p) pos-start)
                        false))
                    (recur (inc count) @(:pos p)))))))))
      trace)))

(defn but [parser & funcs]
  (name* "but"
    (fn but-fn [p]
      (when-not (the-end p)
        (let [pos1 @(:pos p)]
          (when (call p (first funcs))
            (let [pos2 @(:pos p)]
              (vreset! (:pos p) pos1)
              (loop [fs (rest funcs)]
                (if (empty? fs)
                  (do
                    (vreset! (:pos p) pos2)
                    true)
                  (if (call p (first fs))
                    (do
                      (vreset! (:pos p) pos1)
                      false)
                    (recur (rest fs))))))))))
    "but"))

(defn chk [parser type expr]
  (let [trace (if TRACE (str "chk(" type "," (stringify expr) ")") "chk")]
    (name* trace
      (fn chk-fn [p]
        (let [pos @(:pos p)]
          (when (= type "<=")
            (vswap! (:pos p) dec))
          (let [ok (call p expr)]
            (vreset! (:pos p) pos)
            (if (= type "!")
              (not ok)
              ok))))
      trace)))

(defn case* [parser var map]
  (let [trace (if TRACE (str "case(" var "," (stringify map) ")") "case")]
    (name* trace
      (fn case-fn [p]
        (let [rule (get map var)]
          (when-not rule
            (FAIL (str "Can't find '" var "' in:") map))
          (call p rule)))
      trace)))

(defn flip [parser var map]
  (let [value (get map var)]
    (when-not value
      (FAIL (str "Can't find '" var "' in:") map))
    (if (string? value)
      value
      (call parser value "number"))))

(defn set* [parser var expr]
  (let [trace (str "set('" var "'," (stringify expr) ")")]
    (name* trace
      (fn set-fn [p]
        (let [value (call p expr "any")]
          (if (= value -1)
            false
            (let [value (if (= value "auto-detect")
                          (auto-detect p)
                          value)]
              ;; Update state-prev
              (vswap! (:state p)
                     (fn [s]
                       (if (< (count s) 2)
                         s
                         (let [state-prev (nth s (- (count s) 2))]
                           (assoc s (- (count s) 2)
                                  (assoc state-prev (keyword var) value))))))
              ;; Propagate to parent scopes
              (let [state @(:state p)
                    size (count state)]
                (when (not= (:name (nth state (- size 2))) "all")
                  (loop [i 3]
                    (when (< i size)
                      (let [idx (- size i 1)
                            st (nth state idx)]
                        (vswap! (:state p)
                               (fn [s]
                                 (assoc s idx (assoc st (keyword var) value))))
                        (when-not (= (:name st) "s_l_block_scalar")
                          (recur (inc i))))))))
              true))))
      trace)))

(defn max* [parser max-val]
  (let [trace (if TRACE (str "max(" max-val ")") "max")]
    (name* trace
      (fn max-fn [p] true)
      trace)))

(defn exclude [parser rule]
  (name* "exclude"
    (fn exclude-fn [p] true)
    "exclude"))

(defn add [parser x y]
  (let [trace (if TRACE (str "add(" x "," (stringify y) ")") "add")]
    (name* trace
      (fn add-fn [p]
        (let [y-val (if (fn? y) (call p y "number") y)]
          (when-not (number? y-val)
            (FAIL (str "y is '" (stringify y-val) "', not number in 'add'")))
          (+ x y-val)))
      trace)))

(defn sub [parser x y]
  (let [trace (if TRACE (str "sub(" x "," y ")") "sub")]
    (name* trace
      (fn sub-fn [p]
        (- x y))
      trace)))

(def ^:private match-rule
  (name* "match"
    (fn match-fn [p]
      (let [state @(:state p)]
        (loop [i (dec (count state))]
          (when (> i 0)
            (if (:end (nth state i))
              (let [{:keys [beg end]} (nth state i)
                    input @(:input p)]
                ;; Handle case where beg > end (return empty string like JS)
                (if (<= beg end)
                  (subs input beg end)
                  ""))
              (do
                (when (= i 1)
                  (FAIL "Can't find match"))
                (recur (dec i))))))))
    "match"))

(defn match [parser]
  match-rule)

(defn len [parser str-val]
  (name* "len"
    (fn len-fn [p]
      (let [s (if (string? str-val) str-val (call p str-val "string"))]
        (count s)))
    "len"))

(defn ord [parser str-val]
  (name* "ord"
    (fn ord-fn [p]
      (let [s (if (string? str-val) str-val (call p str-val "string"))]
        (- (int (first s)) 48)))
    "ord"))

(defn if* [parser test do-if-true]
  (name* "if"
    (fn if-fn [p]
      (let [test-val (if (instance? Boolean test) test (call p test "boolean"))]
        (if test-val
          (do
            (call p do-if-true)
            true)
          false)))
    "if"))

(defn lt [parser x y]
  (let [trace (if TRACE (str "lt(" (stringify x) "," (stringify y) ")") "lt")]
    (name* trace
      (fn lt-fn [p]
        (let [x-val (if (number? x) x (call p x "number"))
              y-val (if (number? y) y (call p y "number"))]
          (< x-val y-val)))
      trace)))

(defn le [parser x y]
  (let [trace (if TRACE (str "le(" (stringify x) "," (stringify y) ")") "le")]
    (name* trace
      (fn le-fn [p]
        (let [x-val (if (number? x) x (call p x "number"))
              y-val (if (number? y) y (call p y "number"))]
          (<= x-val y-val)))
      trace)))

(def ^:private m-rule
  (name* "m"
    (fn m-fn [p]
      (:m (state-curr p)))
    "m"))

(defn m [parser]
  m-rule)

(def ^:private t-rule
  (name* "t"
    (fn t-fn [p]
      (:t (state-curr p)))
    "t"))

(defn t [parser]
  t-rule)

;; Auto-detect indent
(defn- scan-spaces
  "Index of the first non-space char at or after pos."
  [^String input pos len]
  (loop [i (long pos)]
    (if (and (< i len) (= (.charAt input i) \space))
      (recur (inc i))
      i)))

(defn auto-detect-indent
  "Equivalent to matching ^((?:\\ *(?:\\#.*)?\\n)*)(\\ *) at pos:
  skip leading blank/comment lines (group 1, pre), then count the
  spaces at the start of the next line (group 2, m-raw)."
  [parser n]
  (let [pos (long @(:pos parser))
        ^String input @(:input parser)
        len (.length input)
        in-seq (and (> pos 0)
                    (let [c (.charAt input (dec pos))]
                      (or (= c \-) (= c \?) (= c \:))))
        [pre-len m-raw]
        (loop [line-start pos]
          (let [sp-end (scan-spaces input line-start len)
                ;; A pre line is spaces, an optional #comment, then \n
                nl (cond
                     (and (< sp-end len)
                          (= (.charAt input sp-end) \newline))
                     sp-end

                     (and (< sp-end len)
                          (= (.charAt input sp-end) \#))
                     (let [j (loop [j sp-end]
                               (if (and (< j len)
                                        (not= (.charAt input j) \newline))
                                 (recur (inc j))
                                 j))]
                       (when (< j len) j))

                     :else nil)]
            (if nl
              (recur (inc (long nl)))
              [(- line-start pos) (- sp-end line-start)])))
        m (if (and in-seq (zero? pre-len))
            (if (= n -1) (inc m-raw) m-raw)
            (- m-raw n))]
    (if (< m 0) 0 m)))

(defn auto-detect
  "Auto-detect indentation. Can take n as parameter or get it from state.
  Equivalent to matching ^.*\\n((?:\\ *\\n)*)(\\ *)(.?) at pos: skip the
  rest of the current line, collect all-space lines (group 1, pre; its
  longest line is max-empty), then the indent of the first content line
  (group 2) and whether any content follows on it (group 3)."
  ([parser] (auto-detect parser (:m (state-curr parser))))
  ([parser n]
   (let [^String input @(:input parser)
         pos (long @(:pos parser))
         len (.length input)
         n (or n 0)
         first-nl (loop [i pos]
                    (cond
                      (>= i len) nil
                      (= (.charAt input i) \newline) i
                      :else (recur (inc i))))
         [max-empty g2-len g3?]
         (if first-nl
           (loop [line-start (inc (long first-nl))
                  max-len 0]
             (let [sp-end (scan-spaces input line-start len)]
               (if (and (< sp-end len)
                        (= (.charAt input sp-end) \newline))
                 (recur (inc sp-end) (max max-len (- sp-end line-start)))
                 [max-len (- sp-end line-start) (< sp-end len)])))
           [0 0 false])
         m (if g3?
             (- g2-len n)
             ;; No content line: smallest k such that pre has no run of
             ;; k spaces, minus n and 1; i.e. longest space run minus n
             (- max-empty n))]
     (when (and (> m 0)
                ;; Some all-space line is indented deeper than m + n,
                ;; i.e. spaces found after the detected indent
                (>= max-empty (+ m n 1)))
       (die "Spaces found after indent in auto-detect (5LLU)"))
     (if (zero? m) 1 m))))

;; Main parse function
(defn parse [parser input]
  (let [input (if (or (empty? input) (str/ends-with? input "\n"))
                input
                (str input "\n"))]
    (vreset! (:input parser) input)
    (vreset! (:end parser) (count input))
    (vreset! (:pos parser) 0)
    (vreset! (:state parser) [])
    (vreset! (:memo parser) {})
    (let [prefixes (callback-chain-prefixes
                    (:callbacks @(:receiver parser)))]
      (vreset! (:cb-chains parser) prefixes)
      (vreset! (:cb-roots parser)
               (build-cb-roots @(:receiver parser) prefixes)))

    (when TRACE
      (vreset! (:trace-on parser) (not (trace-start parser))))

    (let [grammar @(requiring-resolve 'yaml-parser.grammar/TOP)]
      (try
        (let [ok (call parser grammar)]
          (trace-flush parser)
          (when-not ok
            (throw (ex-info "Parser failed" {})))
          (when (< @(:pos parser) @(:end parser))
            (throw (ex-info "Parser finished before end of input" {})))
          true)
        (catch #?(:clj Exception :glj go/any) e
          (trace-flush parser)
          (throw e))))))

;; Trace support (stubs for now)
(defn trace-start [parser]
  (or (env "TRACE_START") ""))

(defn trace-flush [parser]
  ;; TODO: implement full trace support
  nil)
