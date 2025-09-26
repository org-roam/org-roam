-include ../config.mk

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.

sharedir ?= $(HOME)/.local/share
infodir  ?= $(sharedir)/info

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref assets/page.css

## Files #############################################################

PKG       = org-roam
PACKAGES  = org-roam

INFOPAGES = $(addsuffix .info,$(PACKAGES))
HTMLFILES = $(addsuffix .html,$(PACKAGES))

## Versions ##########################################################

EMACS_VERSION = 26.1

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

######################################################################

.PHONY: default
default: info

## Build #############################################################

.PHONY: info
info: dir

dir: $(INFOPAGES)
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

.PHONY: html
html: $(HTMLFILES)
	mv $(PKG).html manual.html

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# NOTE: %.org is not a prerequisite since package archives can’t generate texi
# from org. texi is generated on org save and committed to repo
%.texi:
	@echo "Generating $@"
	@$(BATCH) $*.org $(ORG_EVAL)

## Install ###########################################################

.PHONY: install
install: install-info

.PHONY: install-info
install-info: info
	install -p -m 755 -d $(DESTDIR)$(infodir)
	install -p -m 644 $(INFOPAGES) $(DESTDIR)$(infodir)

## Clean #############################################################

.PHONY: clean
clean:
	@echo "Cleaning doc/*..."
	@rm -rf dir $(INFOPAGES) $(HTMLFILES)

## Release management ################################################

# Run VERSION=N make -C doc release-texi to explicitly set version when releasing
.PHONY: release-texi
release-texi:
	VERSION=$(VERSION) $(MAKE) -B $(PKG).texi
