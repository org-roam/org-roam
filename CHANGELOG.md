# Changelog
## TBD
### Breaking
### Added
### Removed
### Fixed
- [#2264](https://github.com/org-roam/org-roam/pull/2264) Support multi-line org titles
- [#2165](https://github.com/org-roam/org-roam/pull/2165) (fix)org-roam-file-p: don't exclude org-roam-directory
- [#2168](https://github.com/org-roam/org-roam/pull/2168) (perf)node-read: filter nodes before mapping --to-candidate
### Changed

## 2.2.2
### Breaking
### Added
- [#2138](https://github.com/org-roam/org-roam/pull/2138) export: add new module
- [#2170](https://github.com/org-roam/org-roam/pull/2170) log: add new module for working with org logs
- [#2158](https://github.com/org-roam/org-roam/pull/2158) db: support emacsql-sqlite-builtin and emacsql-sqlite-module
- [#2160](https://github.com/org-roam/org-roam/pull/2160) core: support a list of `org-roam-file-exclude-regexp`

### Removed
### Fixed
- [#2091](https://github.com/org-roam/org-roam/pull/2091) node: fix org-roam-promote-entire-buffer structural errors
- [#2130](https://github.com/org-roam/org-roam/pull/2130) buffer: unlinked-references section now also searches within symlinked directories
- [#2152](https://github.com/org-roam/org-roam/pull/2152) org-roam-preview-default-function: doesn't copy copy content of next heading node when current node's content is empty
- [#2159](https://github.com/org-roam/org-roam/pull/2159) db: fix db syncs on narrowed buffers
- [#2156](https://github.com/org-roam/org-roam/pull/2157) capture: templates with functions are handled correctly to avoid signaling `char-or-string-p`


### Changed
- [#2160](https://github.com/org-roam/org-roam/pull/2160) core: ignore files in `org-attach-id-dir` by default

## 2.2.1
### Breaking
- [#2054](https://github.com/org-roam/org-roam/pull/2054) node: simplify default `org-roam-node-display-template`.
  This was done so completions work fine by default on all completion systems. To restore the tabular vertical completion interface, set this in your configuration:

  ```emacs-lisp
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  ```

### Added
- [#2042](https://github.com/org-roam/org-roam/pull/2042) db: add `org-roam-db-extra-links-elements` and `org-roam-db-extra-links-exclude-keys` for fine-grained control over additional link parsing
- [#2049](https://github.com/org-roam/org-roam/pull/2049) capture: allow ID to be used as part of `org-roam-capture-templates`
- [#2050](https://github.com/org-roam/org-roam/pull/2050) core: add `FILTER-FN` to `org-roam-node-random`
- [#2065](https://github.com/org-roam/org-roam/pull/2065) dailies: add `keys` argument to the remaining dailies functions `org-roam-dailies-goto-yesterday`/`-today`/`-tomorrow`/`-date` and `org-roam-dailies-capture-yesterday`/`-tomorrow`/`-date` to give the abilty to get into a capture buffer bypassing the selection screen in all dailies commands. Extension of #2055
- [#2079](https://github.com/org-roam/org-roam/pull/2079) capture: ensure that `:ref` info captured in all cases.
- [#2121](https://github.com/org-roam/org-roam/pull/2121) buffer: add unique option to `org-roam-backlinks-section`

### Removed
### Fixed
- [#2086](https://github.com/org-roam/org-roam/pull/2086) capture: correctly update org-id-locations on capture
- [#2082](https://github.com/org-roam/org-roam/pull/2082) buffer: don't destroy window if `org-roam-node-toggle` reuses window
- [#2080](https://github.com/org-roam/org-roam/pull/2080) dailies: prevent multiple "dailies/" subdir expansions
- [#2055](https://github.com/org-roam/org-roam/pull/2055) dailies: removed stray f require, which was causing require and compilation errors
- [#2117](https://github.com/org-roam/org-roam/pull/2117) capture: preserve trailing whitespace content in capture templates

### Changed
- [#2060](https://github.com/org-roam/org-roam/pull/2060) node: added double acute accent normalization for Unicode characters in titles
- [#2040](https://github.com/org-roam/org-roam/pull/2040) completions: fix completions display-width for Helm users
- [#2025](https://github.com/org-roam/org-roam/pull/2025) chore: removed the dependencies on f.el and s.el
- [#2109](https://github.com/org-roam/org-roam/pull/2109) capture: `org-roam-node-insert` places cursor after inserted link where appropriate
- [#2123](https://github.com/org-roam/org-roam/pull/2123), [#2124](https://github.com/org-roam/org-roam/pull/2124) buffer: `org-roam-mode-section-functions` renamed to `org-roam-mode-sections`, supports passing args into the section-rendering function

## 2.2.0
### Added
- [#1806](https://github.com/org-roam/org-roam/pull/1806), [#2017](https://github.com/org-roam/org-roam/pull/2017) db: support caching and usage of Org 9.5's in-built citations
- [#1963](https://github.com/org-roam/org-roam/pull/1963) db: cache file title into files table
- [#1977](https://github.com/org-roam/org-roam/pull/1977) db: support Org-ref v3 citations
- [#1907](https://github.com/org-roam/org-roam/pull/1907), [#2009](https://github.com/org-roam/org-roam/pull/2009), [#2018](https://github.com/org-roam/org-roam/pull/2018) db: support sqlite3
- [#2028](https://github.com/org-roam/org-roam/pull/2028) dailies: add `keys` argument to `org-roam-dailies-capture-today` and `org-roam-dailies--capture` functions to give the abilty to get into a capture buffer bypassing the selection screen

### Removed
### Changed
- [#1795](https://github.com/org-roam/org-roam/pull/1795) buffer: optimized reflinks fetch
- [#1809](https://github.com/org-roam/org-roam/pull/1809) capture: the mandatory `:if-new` property of each capture template is now renamed to `:target`
- [#1829](https://github.com/org-roam/org-roam/pull/1829) perf: file sql updates are now wrapped in a transaction
- [#1877](https://github.com/org-roam/org-roam/pull/1877) dailies: stop asking for time, only date
- [#1949](https://github.com/org-roam/org-roam/pull/1949) db: check for property drawers are now case-insensitive

### Fixed
- [#1798](https://github.com/org-roam/org-roam/pull/1798) org-roam-node-at-point: do not skip invisible headings
- [#1807](https://github.com/org-roam/org-roam/pull/1807) capture: always trigger `:if-new` template for existing nodes
- [#1813](https://github.com/org-roam/org-roam/pull/1813) db: prevent empty ROAM_ALIASES from crashing db updates
- [#1816](https://github.com/org-roam/org-roam/pull/1816) db: prevent invalid ROAM_REFS from crashing db updates
- [#1893](https://github.com/org-roam/org-roam/pull/1893), [#1896](https://github.com/org-roam/org-roam/pull/1896), [#1901](https://github.com/org-roam/org-roam/pull/1901), [#1895](https://github.com/org-roam/org-roam/pull/1895), [#1904](https://github.com/org-roam/org-roam/pull/1904) completions: various mini-buffer related completion fixes
- [#1931](https://github.com/org-roam/org-roam/pull/1931) utils: org-roam-set-keyword now skips over all drawers
- [#1947](https://github.com/org-roam/org-roam/pull/1947) db: links in ROAM_REFS are no longer considered as links
- [#1948](https://github.com/org-roam/org-roam/pull/1948) completions: fix same-line completions
- [#1953](https://github.com/org-roam/org-roam/pull/1953) db: refresh CATEGORY before writing to db
- [#1946](https://github.com/org-roam/org-roam/pull/1946), [#1946](https://github.com/org-roam/org-roam/pull/1946), [#1958](https://github.com/org-roam/org-roam/pull/1958) various performance improvements
- [#1980](https://github.com/org-roam/org-roam/pull/1980) utils: fix org-roam-with-file changing the minor-mode
- [#2016](https://github.com/org-roam/org-roam/pull/2016) db: fix node caching being affected by agenda variables
- [#2033](https://github.com/org-roam/org-roam/pull/2023) db: respect local variables during db parsing

## 2.1.0
### Added
- [#1693](https://github.com/org-roam/org-roam/pull/1693) added `filter-fn` and `templates` parameter to `org-roam-capture`, `org-roam-node-find`, and `org-roam-node-insert`
- [#1709](https://github.com/org-roam/org-roam/pull/1709) added ability to specify default value in org-roam capture templates
- [#1710](https://github.com/org-roam/org-roam/pull/1710) added `org-roam-extract-subtree`
- [#1720](https://github.com/org-roam/org-roam/pull/1720) added `org-roam-db-update-on-save`
- [#1758](https://github.com/org-roam/org-roam/pull/1758) added `org-roam-db-autosync-mode`, replacing `org-roam-setup` and `org-roam-teardown`

### Removed
- [#1716](https://github.com/org-roam/org-roam/pull/1716) helper function `org-roam-get-keyword` is now obsolete: prefer `org-collect-keywords`

### Changed
- [#1595](https://github.com/org-roam/org-roam/pull/1595), [#1724](https://github.com/org-roam/org-roam/pull/1724) Major refactoring and restructuring of the codebase
- [#1655](https://github.com/org-roam/org-roam/pull/1655) improved org-roam contents preview
- [#1741](https://github.com/org-roam/org-roam/pull/1741) expose `org-roam-capture-` keys in interactive commands
- [#1786](https://github.com/org-roam/org-roam/pull/1786) org-roam-version now outputs commit hash if found
- [#1788](https://github.com/org-roam/org-roam/pull/1788) the point is not moved if the node is already visited

### Fixed
- [#1608](https://github.com/org-roam/org-roam/pull/1608) migration: empty ROAM_REFS are now removed
- [#1609](https://github.com/org-roam/org-roam/pull/1609) migration: fixed file-link replacement
- [#1653](https://github.com/org-roam/org-roam/pull/1653) migration: fixed tags migration
- [#1694](https://github.com/org-roam/org-roam/pull/1694) core: nodes with no title are now skipped
- [#1637](https://github.com/org-roam/org-roam/pull/1637) core: fix org-ref multi-cite links not being split
- [#1651](https://github.com/org-roam/org-roam/pull/1651) core: fix org-roam-file-p crashing when there is no corresponding file
- [#1705](https://github.com/org-roam/org-roam/pull/1705) core: fix for add/remove of file-level tags
- [#1769](https://github.com/org-roam/org-roam/pull/1769) core: gracefully handle absence of `org-id-locations-file`
- [#1713](https://github.com/org-roam/org-roam/pull/1713) capture: check for file-existence before template insertion
- [#1725](https://github.com/org-roam/org-roam/pull/1725) db: always compute hash of encrypted file to prevent re-processing of encrypted files

## 2.0.0
### Added
- [#1396](https://github.com/org-roam/org-roam/pull/1396) add option to choose between prepending, appending, and omitting `roam_tags` in file completion
- [#1270](https://github.com/org-roam/org-roam/pull/1270) capture: create OLP if it does not exist. Removes need for OLP setup in `:head`.
- [#1353](https://github.com/org-roam/org-roam/pull/1353) support file-level property drawers

### Changed

- [#1352](https://github.com/org-roam/org-roam/pull/1352) prefer lower-case for roam_tag and roam_alias in interactive commands
- [#1513](https://github.com/org-roam/org-roam/pull/1513) replaced hardcoded "svg" with defcustom org-roam-graph-filetype
- [#1540](https://github.com/org-roam/org-roam/pull/1540) allow `roam_tag` and `roam_alias` to be specified on multiple lines

### Fixed

- [#1281](https://github.com/org-roam/org-roam/pull/1281) fixed idle-timer not instantiated on `org-roam-mode`
- [#1308](https://github.com/org-roam/org-roam/pull/1308) fixed file renames corrupting database
- [#1325](https://github.com/org-roam/org-roam/pull/1325) make titles and tags extracted unique per note
- [#1327](https://github.com/org-roam/org-roam/pull/1327) preserve existing link description during automatic replacement
- [#1346](https://github.com/org-roam/org-roam/pull/1346) prevent malformed path to `org-roam-index-file`
- [#1347](https://github.com/org-roam/org-roam/pull/1347) allow use of `%a` element in regular Org-roam captures
- [#1352](https://github.com/org-roam/org-roam/pull/1352) fixed org-roam-{tag/alias}-{add/delete} altering the original case of the Org property
- [#1374](https://github.com/org-roam/org-roam/pull/1374) fix headline completions erroring out
- [#1375](https://github.com/org-roam/org-roam/pull/1375) fix org-roam-protocol to use existing ref file
- [#1403](https://github.com/org-roam/org-roam/issues/1403) fixed inconsistency between how we write and read props like alias and tags
- [#1409](https://github.com/org-roam/org-roam/issues/1398) prevent inclusion of non-org-roam files in `org-roam-dailies--list-files`
- [#1542](https://github.com/org-roam/org-roam/issues/1542) fix files not excluded when `org-roam-list-files-commands` is nil

## 1.2.3 (13-11-2020)

Primarily a stabilization and bug-fix release.

Org-roam-dailies has also been revamped to include new features, see [this video](https://www.youtube.com/watch?v=1q9x2aZCJJ4) for a quick overview.

### Added

- [#978](https://github.com/org-roam/org-roam/pull/978) Revamp org-roam-dailies
- [#1183](https://github.com/org-roam/org-roam/pull/1183) Interactive functions for managing aliases and tags in Org-roam file, namely `org-roam-alias-add`, `org-roam-alias-delete`, `org-roam-tag-add`, and `org-roam-tag-delete`.
- [#1215](https://github.com/org-roam/org-roam/pull/1215) Multiple `ROAM_KEY` keywords can now be specified in one file. This allows bibliographical entries to share the same note file.
- [#1238](https://github.com/org-roam/org-roam/pull/1238) Add `org-roam-prefer-id-links` variable to select linking method
- [#1239](https://github.com/org-roam/org-roam/pull/1239) Allow `org-roam-protocol` to capture the webpage's selection, and add a toggle for storing the links to the pages
- [#1264](https://github.com/org-roam/org-roam/pull/1264) add `org-roam-db-update-method` to control when the cache is rebuilt.

### Changed

- [#1264](https://github.com/org-roam/org-roam/pull/1264) renamed `org-roam-update-db-idle-seconds` to `org-roam-db-idle-idle-seconds`

### Fixed

- [#1074](https://github.com/org-roam/org-roam/issues/1074) fix `org-roam--extract-links` to handle content boundaries.
- [#1193](https://github.com/org-roam/org-roam/issues/1193) fix `org-roam-db-build-cache` by not killing temporary buffer in `org-roam--extract-links`.
- [#1195](https://github.com/org-roam/org-roam/issues/1195) fix ID face showing as invalid if within Org ID files, but not Org-roam's.
- [#1199](https://github.com/org-roam/org-roam/issues/1199) make Org-roam link insertions respect `org-roam-link-title-format` everywhere.
- [#1201](https://github.com/org-roam/org-roam/issues/1201) fix `org-roam-db-build-cache` failing in scenarios involving duplicate IDs and deleted files.
- [#1226](https://github.com/org-roam/org-roam/issues/1226) only update relative path of file links
- [#1232](https://github.com/org-roam/org-roam/issues/1232) fix incorrect title extractions from narrowed buffers
- [#1233](https://github.com/org-roam/org-roam/issues/1233) fixes bug where descriptive file links become plain links during update for relative paths
- [#1252](https://github.com/org-roam/org-roam/issues/1252) respect original link type during automatic replacement

## 1.2.2 (06-10-2020)

In this release we support fuzzy links of the form `[[roam:Title]]`, `[[roam:*Headline]]` and `[[roam:Title*Headline]]`. Completion for these fuzzy links is supported via `completion-at-point`.

Org-roam now does not resolve symlinks. This significantly speeds up cache builds, but may result in some workflows breaking. In particular, Org-roam now cannot figure out if two distinct file paths in the Org-roam directory are the same file, and both files will be processed as if they were different files. This error seems to be unavoidable now that symlinks are not resolved, but this workflow is rare and should not affect most users.

This change requires you to set `org-roam-directory` to the resolved path of a folder. That is:

```elisp
(setq org-roam-directory (file-truename "/path/to/directory/"))
```

### Breaking Changes

- [#1164](https://github.com/org-roam/org-roam/pull/1164) Org-roam now stores the database in the user's Emacs directory by default
- [#910](https://github.com/org-roam/org-roam/pull/910) Deprecate `company-org-roam`, using `completion-at-point` instead. To use this with company, add the `company-capf` backend instead.
- [#1109](https://github.com/org-roam/org-roam/pull/1109) Org-roam now does not resolve symlinks.

### Features

- [#1163](https://github.com/org-roam/org-roam/pull/1163) Support file-level IDs introduced in Org 9.4
- [#1093](https://github.com/org-roam/org-roam/pull/1093) Add `vanilla` org-roam-tag-source to extract buffer Org tags
- [#1079](https://github.com/org-roam/org-roam/pull/1079) Add `org-roam-tag-face` to customize appearance of tags in interactive commands
- [#1073](https://github.com/org-roam/org-roam/pull/1073) Rename file on title change, when `org-roam-rename-file-on-title-change` is non-nil.
- [#1071](https://github.com/org-roam/org-roam/pull/1071) Update link descriptions on title changes, and clean-up rename file advice
- [#1061](https://github.com/org-roam/org-roam/pull/1061) Speed up the extraction of file properties, headlines, and titles
- [#1046](https://github.com/org-roam/org-roam/pull/1046) New user option to exclude files from Org-roam
- [#1032](https://github.com/org-roam/org-roam/pull/1032) File changes are now propagated to the database on idle timer. This prevents large wait times during file saves.
- [#974](https://github.com/org-roam/org-roam/pull/974) Protect region targeted by `org-roam-insert`
- [#994](https://github.com/org-roam/org-roam/pull/994) Simplify org-roam-store-link
- [#1062](https://github.com/org-roam/org-roam/pull/1062) Variable `org-roam-completions-everywhere` allows for completions everywhere from word at point
- [#910](https://github.com/org-roam/org-roam/pull/910), [#1105](https://github.com/org-roam/org-roam/pull/1105) Support fuzzy links of the form `[[roam:Title]]`, `[[roam:*Headline]]` and `[[roam:Title*Headline]]`

### Bugfixes

- [#1057](https://github.com/org-roam/org-roam/pull/1057) Improve performance of database builds by preventing generation of LaTeX and image previews.
- [#1103](https://github.com/org-roam/org-roam/pull/1103) Fix long build-times for Windows on files with URL links

## 1.2.1 (27-07-2020)

This release consisted of a big deal of refactoring and bug fixes. Notably, we fixed several catastrophic failures on db builds with bad setups (#854), and modularized tag and title extractions.

We also added some new features that had been a long time coming:

1. We made the backlinks more outline-friendly by also showing the outline hierarchy for a backlink (#863)
2. We now support nested captures, which is crucial for `org-roam-protocol` (#966)

### Breaking Changes

- [#908](https://github.com/org-roam/org-roam/pull/908) Normalized titles in database. May break external packages that rely on unnormalized titles.

### Features

- [#814](https://github.com/org-roam/org-roam/pull/814) Implement `org-roam-insert-immediate`
- [#833](https://github.com/org-roam/org-roam/pull/833) Add customization of file titles with `org-roam-title-to-slug-function`.
- [#839](https://github.com/org-roam/org-roam/pull/839) Return selected file from `org-roam-insert`
- [#847](https://github.com/org-roam/org-roam/pull/847) Add GC threshold `org-roam-db-gc-threshold` to temporarily change the threshold on expensive operations.
- [#847](https://github.com/org-roam/org-roam/pull/847) Use sqlite3 transactions instead of storing the values to be inserted.
- [#851](https://github.com/org-roam/org-roam/pull/851) Add `'first-directory'` option for `org-roam-tag-sources`
- [#863](https://github.com/org-roam/org-roam/pull/863) Display outline hierarchy in backlinks buffer
- [#898](https://github.com/org-roam/org-roam/pull/898) Add `org-roam-random-note` to browse a random note.
- [#900](https://github.com/org-roam/org-roam/pull/900) Support and index all valid org links
- [#966](https://github.com/org-roam/org-roam/pull/966) Enable nested captures

### Bugfixes

- [#854](https://github.com/org-roam/org-roam/pull/854) Warn instead of fail when duplicate refs and IDs exist.
- [#857](https://github.com/org-roam/org-roam/pull/857) Fix tag extraction for symlinked directories.
- [#894](https://github.com/org-roam/org-roam/pull/894) Perform link fixes on all Org-roam files
- [#952](https://github.com/org-roam/org-roam/pull/952) Cache `${foo}` template variables so they do not need to be re-entered twice

## 1.2.0 (12-06-2020)

In this release, we improved the linking process by achieving feature parity between links to files and links to headlines. Before, we used the `file:foo::*bar` format to link to the headline `bar` in file `foo`, but this was prone to breakage upon renaming the file or modifying the headline. This is not the case anymore. Now, we use `org-id` to create IDs for those headlines, which are then stored in our database to compute the relationships and jump around. Note that this will work even if you’re not using `org-id` in your global configuration for Org-mode.

This is a major step forward. Supporting the in-file structure of Org-mode files means that we can interface with many of its core-features like TODOs, properties, priorities, etc. UX will have to be figured out, but this release ushers in a new age in terms of functionalities.

We also add `org-roam-unlinked-references`, which naively finds text that could be references to the current Org-roam file.

### Breaking Changes

- [#701](https://github.com/org-roam/org-roam/pull/701) Use `emacsql-sqlite3` instead of `emacsql-sqlite` for better Windows compatibility. This requires the presence of the standard `sqlite3` binary on your machine.
- [#750](https://github.com/org-roam/org-roam/pull/750) Deprecate `org-roam-buffer-no-delete-other-windows` in favour of `org-roam-buffer-window-parameters`.

### Features

- [#787](https://github.com/org-roam/org-roam/pull/787) Add `org-roam-unlinked-references`
- [#783](https://github.com/org-roam/org-roam/pull/783) Add support for headlines
- [#757](https://github.com/org-roam/org-roam/pull/757) Roam global properties are now case-insensitive
- [#680](https://github.com/org-roam/org-roam/pull/680) , [#703](https://github.com/org-roam/org-roam/pull/703), [#708](https://github.com/org-roam/org-roam/pull/708) Add `org-roam-doctor` checkers for `ROAM_*` properties
- [#664](https://github.com/org-roam/org-roam/pull/664) Add support for shelling out to `rg` and `find` in `org-roam--list-files`
- [#679](https://github.com/org-roam/org-roam/pull/679), [#683](https://github.com/org-roam/org-roam/pull/683) Building of the graph now happens in a separate process

### Bugfixes

- [#714](https://github.com/org-roam/org-roam/pull/714) No longer print citelinks in backlinks buffer if there is no `ROAM_KEY` property or `org-ref` is not installed
- [#759](https://github.com/org-roam/org-roam/pull/759), [#760](https://github.com/org-roam/org-roam/pull/760) Tags are only added to the tags table if there are actually tags present
- [#700](https://github.com/org-roam/org-roam/pull/700), [#733](https://github.com/org-roam/org-roam/pull/733) Allow symlinks within the `org-roam` directory

## 1.1.1 (18-05-2020)

In this release, we added two new features:

1. `org-roam-doctor`: a linting system that helps you discover possible problems with your Org-roam files. This is in the spirit of keeping notes in top quality.
2. A tagging system: one can now use sub-directories, and the `#+roam_tags` key add additional meta data to notes. For more information, see [here](https://www.orgroam.com/manual/Tags.html#Tags).

As usual, this release comes with a multitude of bug-fixes and refactorings.

### Breaking Changes

- [#523](https://github.com/org-roam/org-roam/pull/523) remove `org-roam-completion-fuzzy-match` in favor of using completion mechanism's configuration options directly
- [#547](https://github.com/org-roam/org-roam/pull/547) Deprecate `org-roam-db--maybe-update`, in favour of `org-roam-db--update-maybe`
- [#604](https://github.com/org-roam/org-roam/pull/604) Deprecate `org-roam-title-include-subdirs`, `org-roam-title-subdir-format` `org-roam-title-subdir-separator`, for a more general tagging system built on subdirectories

### Bugfixes

- [#509](https://github.com/org-roam/org-roam/pull/509) fix backup files being tracked in database
- [#509](https://github.com/org-roam/org-roam/pull/509) fix external org files being tracked in database
- [#537](https://github.com/org-roam/org-roam/pull/537) quote graphviz node and edge configuration options to allow multi-word configurations
- [#545](https://github.com/org-roam/org-roam/pull/545) fix `org-roam--extract-links` to ensure that multiple citations (`cite:key1,key2`) are split correctly
- [#547](https://github.com/org-roam/org-roam/pull/547) Fix unlinked citations
- [#660](https://github.com/org-roam/org-roam/pull/660) fix rename-file advice not working for renaming to directories
- [#660](https://github.com/org-roam/org-roam/pull/660) fix links breaking within file on file movement

### Features

- [#538](https://github.com/org-roam/org-roam/pull/538) Optionally use text in first headline as title
- [#553](https://github.com/org-roam/org-roam/pull/553) Add prefix argument to `org-roam-db-build-cache` for forcing rebuilds
- [#560](https://github.com/org-roam/org-roam/pull/560) Apply 'error face to distinguish broken links
- [#570](https://github.com/org-roam/org-roam/pull/570) Add `org-roam-doctor` to diagnose org-roam files
- [#625](https://github.com/org-roam/org-roam/pull/625) Add `org-roam-title-sources` to control how titles are retrieved within notes
- [#604](https://github.com/org-roam/org-roam/pull/604) Add a tagging system. `org-roam-tag-sources` controls how tags are retrieved from notes

### Internal Changes

- [#547](https://github.com/org-roam/org-roam/pull/547) Added `type` column to the `refs` table
- [#606](https://github.com/org-roam/org-roam/pull/606) Added `org-roam-dev.el` for developer coding standards
- [#622](https://github.com/org-roam/org-roam/pull/622) Online documentation now points to the Org-based documentation

## 1.1.0 (21-04-2020)

To the average user, this release is mainly a bugfix release with additional options to customize. However, the changes made to the source is significant. Most notably, in this release:

1. The codebase has been modularized into separate files, to ease future maintenance and adding of new features (mainly by [@progfolio](https://github.com/progfolio)). Because of these changes, we had to rename many functions and variables: the old names are kept for backwards compatibility, but you are encouraged to use the new function names. You'll receive a warning when you're calling the function with its obsolete name.
2. [@kljohann](https://github.com/kljohann) did some fantastic work on graph generation: allowing building images for connected components within the graph up to a specified distance
3. We also started supporting `org-ref` natively: cite links now show up in both the graph and the org-roam buffer.

In the coming months, you can expect work on bigger projects (e.g. revamping the org-roam buffer).

### Breaking Changes

- [#385](https://github.com/org-roam/org-roam/pull/385) Deprecate `org-roam-graph-node-shape` in favour of `org-roam-graph-node-extra-config`.
- [#473](https://github.com/org-roam/org-roam/pull/473) Deprecate `org-roam-date-filename-format` and `org-roam-date-title-format`, in favour of `org-roam-dailies-capture-templates`.

### New Features

- [#350](https://github.com/org-roam/org-roam/pull/350) Add `org-roam-db-location` to customize location of org-roam database.
- [#359](https://github.com/org-roam/org-roam/pull/359) Add `org-roam-verbose` to allow or silence printing of information.
- [#374](https://github.com/org-roam/org-roam/pull/374) Add support for `org-ref` `cite:` links
- [#380](https://github.com/org-roam/org-roam/pull/380) Allow `org-roam-buffer-position` to also be `top` or `bottom`
- [#385](https://github.com/org-roam/org-roam/pull/385) Add `org-roam-graph-node-extra-config` to configure Graphviz nodes
- [#398](https://github.com/org-roam/org-roam/pull/398), [#418](https://github.com/org-roam/org-roam/pull/418) Add graph building for connected components
- [#435](https://github.com/org-roam/org-roam/pull/435) Add `org-roam-graph-edge-extra-config` to configure Graphviz edges
- [#439](https://github.com/org-roam/org-roam/pull/439) Add support for `org-ref` citations to display as edges in graph. Add `org-roam-graph-edge-cites-extra-config` to configure these edges
- [#465](https://github.com/org-roam/org-roam/pull/465) Add `org-roam-file-extensions` to allow detection of org files with different file extensions
- [#488](https://github.com/org-roam/org-roam/pull/488) Allow a function for `org-roam-graph-viewer`
- [#491](https://github.com/org-roam/org-roam/pull/491) Use TITLE as description when linking before first heading

### Bugfixes

- [#470](https://github.com/org-roam/org-roam/pull/470) Add workaround for undocumented `file-truename` behaviour in `org-roam--org-roam-file-p`.

### Internal Changes

- [#363](https://github.com/org-roam/org-roam/pull/363), [#473](https://github.com/org-roam/org-roam/pull/473) Modularize org-roam features.
- [#497](https://github.com/org-roam/org-roam/pull/497) Simplify `org-roam--list-files` implementation

## 1.0.0 (23-03-2020)

Org-roam is now on MELPA! We have squashed most of the bugs, and Org-roam has
been stable for the most part.

### New Features

- [#269](https://github.com/org-roam/org-roam/pull/269) Add `org-roam-graphviz-extra-options`
- [#257](https://github.com/org-roam/org-roam/pull/257) Add a company-backend `company-org-roam`
- [#284](https://github.com/org-roam/org-roam/pull/284), [#289](https://github.com/org-roam/org-roam/pull/289) Configurable `org-roam-completion-system` with options `'default`, `'ido`, `'ivy` and `'helm`
- [#289](https://github.com/org-roam/org-roam/pull/289) Add customizable `org-roam-fuzzy-match` to allow fuzzy-matching of candidates
- [#290](https://github.com/org-roam/org-roam/pull/290) Add `org-roam-date-title-format` and `org-roam-date-filename-format` for customizing Org-roam's date files
- [#296](https://github.com/org-roam/org-roam/pull/296) Allow multiple exclusion matchers in `org-roam-graph-exclude-matcher`

### Bugfixes

- [#293](https://github.com/org-roam/org-roam/pull/293) Fix capture templates not working as expected for `org-roam-find-file`
- [#275](https://github.com/org-roam/org-roam/pull/275) Fix database rebuild when `org-roam-directory` is set locally

## 1.0.0-rc1 (06-03-2020)

This is a pre-release before the push to MELPA. It contains large
internal changes, with little user-facing changes. Most notably, the
backing storage has been changed to a SQLite database, and a
templating system using `org-capture` is introduced.

### Breaking Changes

- [#200](https://github.com/org-roam/org-roam/pull/200) Move Org-roam cache into a SQLite database.
- [#203](https://github.com/org-roam/org-roam/pull/203) Roam protocol is deprecated, in favour of extending org-roam-protocol.

### New Features

- [#182](https://github.com/org-roam/org-roam/pull/182) Support file name aliases via `#+ROAM_ALIAS`.
- [#216](https://github.com/org-roam/org-roam/pull/216) Adds templating functionality by extending org-capture.
- [#232](https://github.com/org-roam/org-roam/pull/232) Adds a prefix key to `org-roam-show-graph`, to generate graph without opening it.
- [#233](https://github.com/org-roam/org-roam/pull/233) Adds `org-roam-graph-exclude-matcher`, which allows exclusion of nodes from graph.
- [#247](https://github.com/org-roam/org-roam/pull/247) Add `org-roam-backlink` face, which allows customizing backlinks appearance
- [#259](https://github.com/org-roam/org-roam/pull/259) Add optional initial-prompt to `org-roam-find-file`

### Bugfixes

- [#207](https://github.com/org-roam/org-roam/pull/207), [#221](https://github.com/org-roam/org-roam/pull/221) small bugfixes to Org-roam graph generation
- [#230](https://github.com/org-roam/org-roam/pull/230) remove nonspacing marks from filenames, to prevent cross-platform errors

### New Contributors

- [@acowley][https://github.com/acowley]
- [@teesloane][https://github.com/teesloane]

## 0.1.2 (2020-02-21)

### Breaking Changes

- [#143](https://github.com/org-roam/org-roam/pull/143) `org-roam-mode` is now a global mode. The installation instructions have changed accordingly.
- [#103](https://github.com/org-roam/org-roam/pull/103) Change `org-roam-file-format` to a function: `org-roam-file-name-function` to allow more flexible file name customizaton. Also changes `org-roam-use-timestamp-as-filename` to `org-roam-filename-noconfirm` to better describe what it does.

### New Features

- [#145](https://github.com/org-roam/org-roam/pull/145) `org-roam-show-graph`: Fallback to Emacs SVG viewer
- [#141](https://github.com/org-roam/org-roam/pull/141) add variable `org-roam-new-file-directory` for new Org-roam files
- [#138](https://github.com/org-roam/org-roam/pull/138) add `org-roam-switch-to-buffer`
- [#124](https://github.com/org-roam/org-roam/pull/124), [#141](https://github.com/org-roam/org-roam/pull/141) Maintain cache consistency on file rename and delete
- [#87](https://github.com/org-roam/org-roam/pull/87), [#90](https://github.com/org-roam/org-roam/pull/90) Support encrypted Org files
- [#110](https://github.com/org-roam/org-roam/pull/110) Add prefix to `org-roam-insert`, for inserting titles down-cased
- [#99](https://github.com/org-roam/org-roam/pull/99) Add keybinding so that `<return>` or `mouse-1` in the backlinks buffer visits the source file of the backlink at point

### Changes

- [#108](https://github.com/org-roam/org-roam/pull/108) Locally overwrite the link following behaviour in the org-roam-buffer to open files in the same window `org-roam` was called from

### Bugfixes

- [#86](https://github.com/org-roam/org-roam/pull/86) Fix `org-roam--parse-content` incorrect `:to` computation for nested files
- [#98](https://github.com/org-roam/org-roam/pull/98) Fix `org-roam--find-file` picking up temporary files
- [#136](https://github.com/org-roam/org-roam/pull/136) Misc bugfixes

### Internal

- [#122](https://github.com/org-roam/org-roam/pull/122), [#128](https://github.com/org-roam/org-roam/pull/128) Improve performance of post-command-hook
- [#92](https://github.com/org-roam/org-roam/pull/92), [#105](https://github.com/org-roam/org-roam/pull/105) Add tests for core functionality

### New Contributors

- [@frigge](https://github.com/frigge)
- [@juergenhoetzel](https://github.com/juergenhoetzel)
- [@chip2n](https://github.com/chip2n)
- [@l3kn](https://github.com/l3kn)
- [@jdormit](https://github.com/jdormit)
- [@herbertjones](https://github.com/herbertjones)
- [@CeleritasCelery](https://github.com/CeleritasCelery)
- [@daniel-koudouna](https://github.com/daniel-koudouna)

## 0.1.1 (2020-02-15)

Mostly a documentation/cleanup release.

### New Features

- [#62](https://github.com/org-roam/org-roam/pull/62) Add the options `org-roam-use-timestamps-as-filename` and `org-roam-file-format`, more in documentation.

### Breaking Changes

- [#62](https://github.com/org-roam/org-roam/pull/62) The ID (file-name) workflow is no longer first-class, but a fallback when titles don't exist.

### Changes

- [#66](https://github.com/org-roam/org-roam/pull/66), [#68](https://github.com/org-roam/org-roam/pull/68): Improved the quality of the package in preparation of submission to MELPA
- [#73](https://github.com/org-roam/org-roam/pull/73): Added CI to the project via Github Issues (Thanks [@alphapapa](https://github.com/alphapapa/) for scripts and setup)
- [#69](https://github.com/org-roam/org-roam/pull/69), [#72](https://github.com/org-roam/org-roam/pull/72), [#75](https://github.com/org-roam/org-roam/pull/75): Major cleanup and de-duplication of code

### Bugfixes

- [#67](https://github.com/org-roam/org-roam/pull/67): Fixed `org-roam--make-file` not creating files with extensions
- [#71](https://github.com/org-roam/org-roam/pull/71), [#78](https://github.com/org-roam/org-roam/pull/78): Fixed `org-roam-insert` not inserting correct paths
- [#82](https://github.com/org-roam/org-roam/pull/82): Fixed nested Org-roam files not being detected as part of Org-roam

<!-- Local Variables: -->
<!-- eval: (auto-fill-mode -1) -->
<!-- End: -->
