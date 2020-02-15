# Changelog

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

 # Local Variables:
 # eval: (auto-fill-mode -1)
 # End:
