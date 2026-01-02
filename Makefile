# This Makefile is mostly a convenience for CI and distributions. Feel free to
# invoke eldev directly locally.
#
# See https://emacs-eldev.github.io/eldev/#installation for Eldev installation.
eldev-opts ?= --color=always --time --trace --debug --loading=packaged --unstable
eldev-bin ?= eldev
eldev := $(eldev-bin) $(eldev-opts)

.PHONY: eldev-check
eldev-check:
	@command -v $(eldev-bin) >/dev/null || { echo "eldev command not found. See https://emacs-eldev.github.io/eldev/#installation"; exit 1; }

.PHONY: clean
clean: eldev-check
	$(eldev) clean all
	$(MAKE) -C doc clean

.PHONY: prepare
prepare: eldev-check
	$(eldev) prepare

.PHONY: lint
lint: eldev-check
	$(eldev) lint

.PHONY: test
test: eldev-check
	$(eldev) test

.PHONY: docs
docs:
	$(MAKE) -C doc

.PHONY: html
html:
	$(MAKE) -C doc html

.PHONY: install
install: install-docs

.PHONY: install-docs
install-docs:
	$(MAKE) -C doc install
