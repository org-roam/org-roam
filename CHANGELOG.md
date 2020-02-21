# Changelog

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

 # Local Variables:
 # eval: (auto-fill-mode -1)
 # End:
