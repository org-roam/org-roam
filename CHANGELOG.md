# Changelog

## 1.1.1 (TBD)

### Bugfixes
* [#509](https://github.com/jethrokuan/org-roam/pull/509) fix backup files being tracked in database
* [#509](https://github.com/jethrokuan/org-roam/pull/509) fix external org files being tracked in database

## 1.1.0 (21-04-2020)

To the average user, this release is mainly a bugfix release with additional options to customize. However, the changes made to the source is significant. Most notably, in this release:

1. The codebase has been modularized into separate files, to ease future maintenance and adding of new features (mainly by [@progfolio](https://github.com/progfolio)). Because of these changes, we had to rename many functions and variables: the old names are kept for backwards compatibility, but you are encouraged to use the new function names. You'll receive a warning when you're calling the function with its obsolete name.
2. [@kljohann](https://github.com/kljohann) did some fantastic work on graph generation: allowing building images for connected components within the graph up to a specified distance
3. We also started supporting `org-ref` natively: cite links now show up in both the graph and the org-roam buffer.

In the coming months, you can expect work on bigger projects (e.g. revamping the org-roam buffer). 
 
### Breaking Changes
* [#385](https://github.com/jethrokuan/org-roam/pull/385) Deprecate `org-roam-graph-node-shape` in favour of `org-roam-graph-node-extra-config`.
* [#473](https://github.com/jethrokuan/org-roam/pull/473) Deprecate `org-roam-date-filename-format` and `org-roam-date-title-format`, in favour of `org-roam-dailies-capture-templates`.

### New Features
* [#350](https://github.com/jethrokuan/org-roam/pull/350) Add `org-roam-db-location` to customize location of org-roam database.
* [#359](https://github.com/jethrokuan/org-roam/pull/359) Add `org-roam-verbose` to allow or silence printing of information.
* [#374](https://github.com/jethrokuan/org-roam/pull/374) Add support for `org-ref` `cite:` links
* [#380](https://github.com/jethrokuan/org-roam/pull/380) Allow `org-roam-buffer-position` to also be `top` or `bottom`
* [#385](https://github.com/jethrokuan/org-roam/pull/385) Add `org-roam-graph-node-extra-config` to configure Graphviz nodes
* [#398](https://github.com/jethrokuan/org-roam/pull/398), [#418](https://github.com/jethrokuan/org-roam/pull/418) Add graph building for connected components
* [#435](https://github.com/jethrokuan/org-roam/pull/435) Add `org-roam-graph-edge-extra-config` to configure Graphviz edges
* [#439](https://github.com/jethrokuan/org-roam/pull/439) Add support for `org-ref` citations to display as edges in graph. Add `org-roam-graph-edge-cites-extra-config` to configure these edges
* [#465](https://github.com/jethrokuan/org-roam/pull/465) Add `org-roam-file-extensions` to allow detection of org files with different file extensions
* [#488](https://github.com/jethrokuan/org-roam/pull/488) Allow a function for `org-roam-graph-viewer`
* [#491](https://github.com/jethrokuan/org-roam/pull/491) Use TITLE as description when linking before first heading

### Bugfixes
* [#470](https://github.com/jethrokuan/org-roam/pull/470) Add workaround for undocumented `file-truename` behaviour in `org-roam--org-roam-file-p`.

### Internal Changes
* [#363](https://github.com/jethrokuan/org-roam/pull/363), [#473](https://github.com/jethrokuan/org-roam/pull/473) Modularize org-roam features.
* [#497](https://github.com/jethrokuan/org-roam/pull/497) Simplify `org-roam--list-files` implementation

## 1.0.0 (23-03-2020)

Org-roam is now on MELPA! We have squashed most of the bugs, and Org-roam has
been stable for the most part.

### New Features
* [#269](https://github.com/jethrokuan/org-roam/pull/269) Add `org-roam-graphviz-extra-options`
* [#257](https://github.com/jethrokuan/org-roam/pull/257) Add a company-backend `company-org-roam`
* [#284](https://github.com/jethrokuan/org-roam/pull/284), [#289](https://github.com/jethrokuan/org-roam/pull/289) Configurable `org-roam-completion-system` with options `'default`, `'ido`, `'ivy` and `'helm`
* [#289](https://github.com/jethrokuan/org-roam/pull/289) Add customizable `org-roam-fuzzy-match` to allow fuzzy-matching of candidates
* [#290](https://github.com/jethrokuan/org-roam/pull/290) Add `org-roam-date-title-format` and `org-roam-date-filename-format` for customizing Org-roam's date files
* [#296](https://github.com/jethrokuan/org-roam/pull/296) Allow multiple exclusion matchers in `org-roam-graph-exclude-matcher`

### Bugfixes
* [#293](https://github.com/jethrokuan/org-roam/pull/293) Fix capture templates not working as expected for `org-roam-find-file`
* [#275](https://github.com/jethrokuan/org-roam/pull/275) Fix database rebuild when `org-roam-directory` is set locally

## 1.0.0-rc1 (06-03-2020)

This is a pre-release before the push to MELPA. It contains large
internal changes, with little user-facing changes. Most notably, the
backing storage has been changed to a SQLite database, and a
templating system using `org-capture` is introduced.

### Breaking Changes
* [#200](https://github.com/jethrokuan/org-roam/pull/200) Move Org-roam cache into a SQLite database.
* [#203](https://github.com/jethrokuan/org-roam/pull/203) Roam protocol is deprecated, in favour of extending org-roam-protocol.

### New Features
* [#182](https://github.com/jethrokuan/org-roam/pull/182) Support file name aliases via `#+ROAM_ALIAS`.
* [#216](https://github.com/jethrokuan/org-roam/pull/216) Adds templating functionality by extending org-capture.
* [#232](https://github.com/jethrokuan/org-roam/pull/232) Adds a prefix key to `org-roam-show-graph`, to generate graph without opening it.
* [#233](https://github.com/jethrokuan/org-roam/pull/233) Adds `org-roam-graph-exclude-matcher`, which allows exclusion of nodes from graph.
* [#247](https://github.com/jethrokuan/org-roam/pull/247) Add `org-roam-backlink` face, which allows customizing backlinks appearance
* [#259](https://github.com/jethrokuan/org-roam/pull/259) Add optional initial-prompt to `org-roam-find-file`

### Bugfixes
* [#207](https://github.com/jethrokuan/org-roam/pull/207), [#221](https://github.com/jethrokuan/org-roam/pull/221) small bugfixes to Org-roam graph generation
* [#230](https://github.com/jethrokuan/org-roam/pull/230) remove nonspacing marks from filenames, to prevent cross-platform errors

### New Contributors
* [@acowley][https://github.com/acowley]
* [@teesloane][https://github.com/teesloane]

## 0.1.2 (2020-02-21)

### Breaking Changes
* [#143](https://github.com/jethrokuan/org-roam/pull/143) `org-roam-mode` is now a global mode. The installation instructions have changed accordingly.
* [#103](https://github.com/jethrokuan/org-roam/pull/103) Change `org-roam-file-format` to a function: `org-roam-file-name-function` to allow more flexible file name customizaton. Also changes `org-roam-use-timestamp-as-filename` to `org-roam-filename-noconfirm` to better describe what it does.

### New Features
* [#145](https://github.com/jethrokuan/org-roam/pull/145) `org-roam-show-graph`: Fallback to Emacs SVG viewer
* [#141](https://github.com/jethrokuan/org-roam/pull/141) add variable `org-roam-new-file-directory` for new Org-roam files
* [#138](https://github.com/jethrokuan/org-roam/pull/138) add `org-roam-switch-to-buffer`
* [#124](https://github.com/jethrokuan/org-roam/pull/124), [#141](https://github.com/jethrokuan/org-roam/pull/141) Maintain cache consistency on file rename and delete
* [#87](https://github.com/jethrokuan/org-roam/pull/87), [#90](https://github.com/jethrokuan/org-roam/pull/90) Support encrypted Org files
* [#110](https://github.com/jethrokuan/org-roam/pull/110) Add prefix to `org-roam-insert`, for inserting titles down-cased
* [#99](https://github.com/jethrokuan/org-roam/pull/99) Add keybinding so that `<return>` or `mouse-1` in the backlinks buffer visits the source file of the backlink at point

### Changes

* [#108](https://github.com/jethrokuan/org-roam/pull/108) Locally overwrite the link following behaviour in the org-roam-buffer to open files in the same window `org-roam` was called from

### Bugfixes
* [#86](https://github.com/jethrokuan/org-roam/pull/86) Fix `org-roam--parse-content` incorrect `:to` computation for nested files
* [#98](https://github.com/jethrokuan/org-roam/pull/98) Fix `org-roam--find-file` picking up temporary files
* [#136](https://github.com/jethrokuan/org-roam/pull/136) Misc bugfixes

### Internal
* [#122](https://github.com/jethrokuan/org-roam/pull/122), [#128](https://github.com/jethrokuan/org-roam/pull/128) Improve performance of post-command-hook
* [#92](https://github.com/jethrokuan/org-roam/pull/92), [#105](https://github.com/jethrokuan/org-roam/pull/105) Add tests for core functionality

### New Contributors
* [@frigge](https://github.com/frigge)
* [@juergenhoetzel](https://github.com/juergenhoetzel)
* [@chip2n](https://github.com/chip2n)
* [@l3kn](https://github.com/l3kn)
* [@jdormit](https://github.com/jdormit)
* [@herbertjones](https://github.com/herbertjones)
* [@CeleritasCelery](https://github.com/CeleritasCelery)
* [@daniel-koudouna](https://github.com/daniel-koudouna)

## 0.1.1 (2020-02-15)

Mostly a documentation/cleanup release.

### New Features
* [#62](https://github.com/jethrokuan/org-roam/pull/62) Add the options `org-roam-use-timestamps-as-filename` and `org-roam-file-format`, more in documentation.

### Breaking Changes
* [#62](https://github.com/jethrokuan/org-roam/pull/62) The ID (file-name) workflow is no longer first-class, but a fallback when titles don't exist.

### Changes
* [#66](https://github.com/jethrokuan/org-roam/pull/66), [#68](https://github.com/jethrokuan/org-roam/pull/68): Improved the quality of the package in preparation of submission to MELPA
* [#73](https://github.com/jethrokuan/org-roam/pull/73): Added CI to the project via Github Issues (Thanks [@alphapapa](https://github.com/alphapapa/) for scripts and setup)
* [#69](https://github.com/jethrokuan/org-roam/pull/69), [#72](https://github.com/jethrokuan/org-roam/pull/72), [#75](https://github.com/jethrokuan/org-roam/pull/75): Major cleanup and de-duplication of code

### Bugfixes
* [#67](https://github.com/jethrokuan/org-roam/pull/67): Fixed `org-roam--make-file` not creating files with extensions
* [#71](https://github.com/jethrokuan/org-roam/pull/71), [#78](https://github.com/jethrokuan/org-roam/pull/78): Fixed `org-roam-insert` not inserting correct paths
* [#82](https://github.com/jethrokuan/org-roam/pull/82): Fixed nested Org-roam files not being detected as part of Org-roam

<!-- Local Variables: -->
<!-- eval: (auto-fill-mode -1) -->
<!-- End: -->
