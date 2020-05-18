-include ../config.mk
include ../default.mk

###################################################################
MANUAL_HTML_ARGS = --css-ref assets/page.css

.PHONY: texi install clean AUTHORS.md stats

all: info

## Build #############################################################

info: $(INFOPAGES) dir
html: $(HTMLFILES)
pdf:  $(PDFFILES)
epub: $(EPUBFILES)

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: org-roam.info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: 
	@printf "Generating org-roam/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) org-roam.texi
	mv org-roam manual
	cp -r assets manual
	cp -r images manual

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

%.epub: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --docbook $< -o epub.xml
	@xsltproc $(DOCBOOK_XSL) epub.xml 2> /dev/null
	@echo "application/epub+zip" > mimetype
	@zip -X --quiet --recurse-paths -0 $@ mimetype
	@zip -X --quiet --recurse-paths -9 --no-dir-entries $@ META-INF OEBPS
	@$(RMDIR) $(EPUBTRASH)

## Install ###########################################################

install: install-info install-docs

install-docs: install-info
	@$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

install-info: info
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $(INFOPAGES) $(DESTDIR)$(infodir)

## Clean #############################################################

clean:
	@printf "Cleaning doc/*...\n"
	@$(RMDIR) dir $(INFOPAGES) $(HTMLFILES) $(HTMLDIRS) $(PDFFILES)
	@$(RMDIR) $(EPUBFILES) $(EPUBTRASH)

## Release management ################################################

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH)
ORG_ARGS += -l ox-extra -l ox-texinfo+
ORG_ARGS += --eval "(or (require 'org-man nil t) (require 'ol-man))"
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
texi:
	@$(EMACSBIN) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

stats:
	@printf "Generating statistics\n"
	@gitstats -c style=/assets/stats.css -c max_authors=999 $(TOP) $(statsdir)

authors: AUTHORS.md

AUTHORS.md:
	@printf "Generating AUTHORS.md..."
	@test -e $(TOP).git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"

# Templates ##########################################################

define AUTHORS_HEADER
Authors
=======

The following people have contributed to Org-Roam.

Names below are sorted alphabetically.

Author
------

- Jethro Kuan <jethrokuan95@gmail.com>

Maintainers
----------

- Jethro Kuan <jethrokuan95@gmail.com>
- Leo Vivier <leo.vivier+dev@gmail.com>

Contributors
------------

endef
export AUTHORS_HEADER
