# yaml-reference-parser-clj

Pure Clojure YAML 1.2 reference parser, packaged for Maven/Clojars as:

```clojure
org.yamlstar/yaml-parser {:mvn/version "0.1.0"}
```

Leiningen:

```clojure
[org.yamlstar/yaml-parser "0.1.0"]
```

## Usage

```clojure
(require '[yaml-parser.core :as yaml-parser])

(yaml-parser/parse "name: YAML\nitems: [1, 2]\n")
```

The parser returns YAML event maps such as `stream_start`, `document_start`,
`mapping_start`, `scalar`, and `stream_end`.

## Development

```sh
make deps
make grammar
make test
make jar
VERSION=0.1.0 make deploy
```

The Makefile uses https://github.com/makeplus/makes to install local `bb`,
`clojure`, `lein`, `maven`, and `perl` tools under `.cache/local`.
`make test` runs both focused API tests and the upstream `yaml-test-suite`
TAP runner. `make deploy` publishes to Clojars through `deps-deploy`; `make deploy-lein`
uses Leiningen.
