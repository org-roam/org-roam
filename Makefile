.PHONY: clean
clean:
	eldev clean all
	$(MAKE) -C doc clean

.PHONY: prepare
prepare:
	eldev -C --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev -C --unstable -T lint

.PHONY: test
test:
	eldev -C --unstable -T test

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
