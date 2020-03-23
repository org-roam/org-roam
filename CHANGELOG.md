# Changelog

## 1.0.0 (23-03-2020)

Org-roam is now on MELPA! We have squashed most of the bugs, and Org-roam has
been stable for the most part.

## New Features
* [#269][gh-269] Add `org-roam-graphviz-extra-options`
* [#257][gh-257] Add a company-backend `company-org-roam`
* [#284][gh-284], [#289][gh-289] Configurable `org-roam-completion-system` with options `'default`, `'ido`, `'ivy` and `'helm`
* [#289][gh-289] Add customizable `org-roam-fuzzy-match` to allow fuzzy-matching of candidates
* [#290][gh-290] Add `org-roam-date-title-format` and `org-roam-date-filename-format` for customizing Org-roam's date files
* [#296][gh-296] Allow multiple exclusion matchers in `org-roam-graph-exclude-matcher`

## Bugfixes
* [#293][gh-293] Fix capture templates not working as expected for `org-roam-find-file`
* [#275][gh-275] Fix database rebuild when `org-roam-directory` is set locally

## 1.0.0-rc1 (06-03-2020)

This is a pre-release before the push to MELPA. It contains large
internal changes, with little user-facing changes. Most notably, the
backing storage has been changed to a SQLite database, and a
templating system using `org-capture` is introduced.

### Breaking Changes
* [#200][gh-200] Move Org-roam cache into a SQLite database.
* [#203][gh-203] Roam protocol is deprecated, in favour of extending org-roam-protocol.

### New Features
* [#182][gh-182] Support file name aliases via `#+ROAM_ALIAS`.
* [#216][gh-216] Adds templating functionality by extending org-capture.
* [#232][gh-232] Adds a prefix key to `org-roam-show-graph`, to generate graph without opening it.
* [#233][gh-233] Adds `org-roam-graph-exclude-matcher`, which allows exclusion of nodes from graph.
* [#247][gh-247] Add `org-roam-backlink` face, which allows customizing backlinks appearance
* [#259][gh-259] Add optional initial-prompt to `org-roam-find-file`

### Bugfixes
* [#207][gh-207], [#221][gh-221] small bugfixes to Org-roam graph generation
* [#230][gh-230] remove nonspacing marks from filenames, to prevent cross-platform errors

### New Contributors
* [@acowley][https://github.com/acowley]
* [@teesloane][https://github.com/teesloane]

## 0.1.2 (2020-02-21)

### Breaking Changes
* [#143][gh-143] `org-roam-mode` is now a global mode. The installation instructions have changed accordingly.
* [#103][gh-103] Change `org-roam-file-format` to a function: `org-roam-file-name-function` to allow more flexible file name customizaton. Also changes `org-roam-use-timestamp-as-filename` to `org-roam-filename-noconfirm` to better describe what it does.

### New Features
* [#145][gh-145] `org-roam-show-graph`: Fallback to Emacs SVG viewer
* [#141][gh-141] add variable `org-roam-new-file-directory` for new Org-roam files
* [#138][gh-138] add `org-roam-switch-to-buffer`
* [#124][gh-124], [#141][gh-141] Maintain cache consistency on file rename and delete
* [#87][gh-87], [#90][gh-90] Support encrypted Org files
* [#110][gh-110] Add prefix to `org-roam-insert`, for inserting titles down-cased
* [#99][gh-99] Add keybinding so that `<return>` or `mouse-1` in the backlinks buffer visits the source file of the backlink at point

### Changes

* [#108][gh-108] Locally overwrite the link following behaviour in the org-roam-buffer to open files in the same window `org-roam` was called from

### Bugfixes
* [#86][gh-86] Fix `org-roam--parse-content` incorrect `:to` computation for nested files
* [#98][gh-98] Fix `org-roam--find-file` picking up temporary files
* [#136][gh-136] Misc bugfixes

### Internal
* [#122][gh-122], [#128][gh-128] Improve performance of post-command-hook
* [#92][gh-92], [#105][gh-105] Add tests for core functionality

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
* [#62][gh-62] Add the options `org-roam-use-timestamps-as-filename` and `org-roam-file-format`, more in documentation.

### Breaking Changes
* [#62][gh-62] The ID (file-name) workflow is no longer first-class, but a fallback when titles don't exist.

### Changes
* [#66][gh-66], [#68][gh-68]: Improved the quality of the package in preparation of submission to MELPA
* [#73][gh-73]: Added CI to the project via Github Issues (Thanks [@alphapapa](https://github.com/alphapapa/) for scripts and setup)
* [#69][gh-69], [#72][gh-72], [#75][gh-75]: Major cleanup and de-duplication of code

### Bugfixes
* [#67][gh-67]: Fixed `org-roam--make-file` not creating files with extensions
* [#71][gh-71], [#78][gh-78]: Fixed `org-roam-insert` not inserting correct paths
* [#82][gh-82]: Fixed nested Org-roam files not being detected as part of Org-roam

[gh-62]: https://github.com/jethrokuan/org-roam/pull/66
[gh-66]: https://github.com/jethrokuan/org-roam/pull/66
[gh-67]: https://github.com/jethrokuan/org-roam/pull/67
[gh-68]: https://github.com/jethrokuan/org-roam/pull/68
[gh-69]: https://github.com/jethrokuan/org-roam/pull/69
[gh-71]: https://github.com/jethrokuan/org-roam/pull/71
[gh-72]: https://github.com/jethrokuan/org-roam/pull/72
[gh-73]: https://github.com/jethrokuan/org-roam/pull/73
[gh-75]: https://github.com/jethrokuan/org-roam/pull/75
[gh-78]: https://github.com/jethrokuan/org-roam/pull/78
[gh-82]: https://github.com/jethrokuan/org-roam/pull/82
[gh-86]: https://github.com/jethrokuan/org-roam/pull/86
[gh-87]: https://github.com/jethrokuan/org-roam/pull/87
[gh-90]: https://github.com/jethrokuan/org-roam/pull/90
[gh-92]: https://github.com/jethrokuan/org-roam/pull/92
[gh-98]: https://github.com/jethrokuan/org-roam/pull/98
[gh-99]: https://github.com/jethrokuan/org-roam/pull/99
[gh-103]: https://github.com/jethrokuan/org-roam/pull/103
[gh-105]: https://github.com/jethrokuan/org-roam/pull/105
[gh-108]: https://github.com/jethrokuan/org-roam/pull/108
[gh-110]: https://github.com/jethrokuan/org-roam/pull/110
[gh-122]: https://github.com/jethrokuan/org-roam/pull/122
[gh-124]: https://github.com/jethrokuan/org-roam/pull/124
[gh-128]: https://github.com/jethrokuan/org-roam/pull/128
[gh-136]: https://github.com/jethrokuan/org-roam/pull/136
[gh-138]: https://github.com/jethrokuan/org-roam/pull/138
[gh-141]: https://github.com/jethrokuan/org-roam/pull/141
[gh-142]: https://github.com/jethrokuan/org-roam/pull/142
[gh-143]: https://github.com/jethrokuan/org-roam/pull/143
[gh-182]: https://github.com/jethrokuan/org-roam/pull/182
[gh-188]: https://github.com/jethrokuan/org-roam/pull/188
[gh-200]: https://github.com/jethrokuan/org-roam/pull/200
[gh-207]: https://github.com/jethrokuan/org-roam/pull/207
[gh-216]: https://github.com/jethrokuan/org-roam/pull/216
[gh-221]: https://github.com/jethrokuan/org-roam/pull/221
[gh-230]: https://github.com/jethrokuan/org-roam/pull/230
[gh-247]: https://github.com/jethrokuan/org-roam/pull/247
[gh-257]: https://github.com/jethrokuan/org-roam/pull/257
[gh-259]: https://github.com/jethrokuan/org-roam/pull/259
[gh-269]: https://github.com/jethrokuan/org-roam/pull/269
[gh-275]: https://github.com/jethrokuan/org-roam/pull/275
[gh-284]: https://github.com/jethrokuan/org-roam/pull/284
[gh-289]: https://github.com/jethrokuan/org-roam/pull/289
[gh-290]: https://github.com/jethrokuan/org-roam/pull/290
[gh-293]: https://github.com/jethrokuan/org-roam/pull/293
[gh-296]: https://github.com/jethrokuan/org-roam/pull/296

 # Local Variables:
 # eval: (auto-fill-mode -1)
 # End:
