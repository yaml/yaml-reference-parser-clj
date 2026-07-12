R := https://github.com/makeplus/makes
M := .cache/makes
$(shell [ -d '$M' ] || git clone -q $R '$M')
YAMLSCRIPT-VERSION := 0.2.24
include $M/init.mk
include $M/babashka.mk
include $M/clojure.mk
include $M/lein.mk
include $M/yamlscript.mk
include $M/perl.mk
include $M/clean.mk
include $M/shell.mk

export HOME := $(LOCAL-HOME)

TEST-SUITE := test/suite

YAML-GRAMMAR-CLJ := src/yaml_parser/grammar.cljc
YAML-GRAMMAR-YAML := share/yaml-spec-1.2-patched.yaml

MAKES-CLEAN := \
  target \
  .cpcache \

MAKES-REALCLEAN := \
  $(TEST-SUITE) \


grammar: $(YAML-GRAMMAR-CLJ)

$(YAML-GRAMMAR-CLJ): $(YAML-GRAMMAR-YAML) util/generate-yaml-grammar $(YS)
	util/generate-yaml-grammar \
	  --from $< \
	  --namespace yaml-parser > $@

test: unit-test suite-test

unit-test: $(BB)
	$(BB) -cp src:test -m yaml-parser.core-test

suite-test: $(BB) $(PERL) grammar $(TEST-SUITE)
	BABASHKA_CLASSPATH=src:test prove -v test/*.t

$(TEST-SUITE):
	mkdir -p $@
	git -C $@ init --quiet
	git -C $@ remote add origin https://github.com/yaml/yaml-test-suite
	git -C $@ fetch origin main
	git -C $@ reset --hard FETCH_HEAD

jar: $(PERL) $(CLOJURE)
	$(CLOJURE) -T:build jar

deploy: $(PERL) $(CLOJURE)
	$(CLOJURE) -T:build deploy

release: $(PERL) $(CLOJURE)
	@[ -n "$(VERSION)" ] || { echo "usage: make release VERSION=x.y.z"; exit 1; }
	@[ -n "$$CLOJARS_USERNAME" ] && [ -n "$$CLOJARS_PASSWORD" ] || \
	  { echo "CLOJARS_USERNAME and CLOJARS_PASSWORD must be set"; exit 1; }
	@git diff --quiet && git diff --cached --quiet || \
	  { echo "git tree not clean"; exit 1; }
	$(MAKE) test
	$(CLOJURE) -T:build deploy
	git tag v$(VERSION)
	@echo "Released $(VERSION) to Clojars. Now run: git push origin main v$(VERSION)"

deploy-lein: $(PERL) $(LEIN)
	$(LEIN) deploy clojars
