TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

sharedir ?= $(HOME)/.local/share
lispdir  ?= $(sharedir)/emacs/site-lisp/org-roam
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/org-roam
statsdir ?= $(TOP)/doc/stats

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

## Files #############################################################

PKG       = org-roam
PACKAGES  = org-roam

TEXIPAGES = $(addsuffix .texi,$(PACKAGES))
INFOPAGES = $(addsuffix .info,$(PACKAGES))
HTMLFILES = $(addsuffix .html,$(PACKAGES))
HTMLDIRS  = $(PACKAGES)
PDFFILES  = $(addsuffix .pdf,$(PACKAGES))
EPUBFILES = $(addsuffix .epub,$(PACKAGES))

ELS = org-roam.el
ELS += org-roam-capture.el
ELS += org-roam-compat.el
ELS += org-roam-db.el
ELS += org-roam-mode.el
ELS += org-roam-node.el
ELS += org-roam-utils.el
ELS += extensions/org-roam-dailies.el
ELS += extensions/org-roam-graph.el
ELS += extensions/org-roam-overlay.el
ELS += extensions/org-roam-protocol.el
ELCS = $(ELS:.el=.elc)
ELMS = org-roam.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = org-roam-autoloads.el org-roam-version.el

## Versions ##########################################################

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS_VERSION = 26.1

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

ifndef LOAD_PATH

ELPA_DIR ?= $(HOME)/.emacs.d/elpa

SYSTYPE := $(shell $(EMACSBIN) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)

# When making changes here, then don't forget to adjust "Makefile",
# ".travis.yml", ".github/ISSUE_TEMPLATE/bug_report.md",
# `magit-emacs-Q-command' and the "Installing from the Git Repository"
# info node accordingly.  Also don't forget to "rgrep \b<pkg>\b".

endif # ifndef LOAD_PATH

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = $(LOAD_PATH)
ORG_LOAD_PATH += -L $(TOP)../ox-texinfo-plus
ORG_LOAD_PATH += -L $(TOP)../org-mode/contrib/lisp
ORG_LOAD_PATH += -L $(TOP)../org-mode/lisp
endif

## Publish ###########################################################

PUBLISH_TARGETS ?= html html-dir pdf epub

DOCBOOK_XSL ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl

EPUBTRASH = epub.xml META-INF OEBPS
