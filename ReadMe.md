# yaml-reference-parser-clj

Pure Clojure YAML 1.2 reference parser, packaged for Maven/Clojars as:

```clojure
org.yamlstar/yaml-parser {:mvn/version "0.2.0"}
```

Leiningen:

```clojure
[org.yamlstar/yaml-parser "0.2.0"]
```

## Usage

```clojure
(require '[yaml-parser.core :as yaml-parser])

(yaml-parser/parse "name: YAML\nitems: [1, 2]\n")
```

The parser returns YAML event maps such as `stream_start`, `document_start`,
`mapping_start`, `scalar`, and `stream_end`.

## Performance

The grammar is generated directly from the machine-readable YAML 1.2 spec
and keeps its ordered-choice structure for spec parity.
The engine makes that fast with packrat memoization of the flow-context
rules, so the backtracking that is inherent to the spec grammar costs a
table lookup instead of a re-parse.
Set `YAML_PARSER_NO_MEMO=1` to disable memoization (useful for A/B
verification that event streams are identical).
`bench/run-bench` times the parser over a small synthetic corpus.

## Development

```sh
make deps
make grammar
make test
make jar
VERSION=0.2.0 make deploy
```

The Makefile uses https://github.com/makeplus/makes to install local `bb`,
`clojure`, `lein`, `maven`, and `perl` tools under `.cache/local`.
`make test` runs both focused API tests and the upstream `yaml-test-suite`
TAP runner. `make deploy` publishes to Clojars through `deps-deploy`; `make deploy-lein`
uses Leiningen.
