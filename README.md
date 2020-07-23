[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![GitHub Release](https://img.shields.io/github/v/release/org-roam/org-roam)](https://img.shields.io/github/v/release/org-roam/org-roam)
[![MELPA](https://melpa.org/packages/org-roam-badge.svg)](https://melpa.org/#/org-roam)

## Synopsis

Org-roam is a [Roam][roamresearch] replica built on top of the
all-powerful [Org-mode][org].

Org-roam is a solution for effortless non-hierarchical note-taking
with Org-mode. With Org-roam, notes flow naturally, making note-taking
fun and easy. Org-roam should also work as a plug-and-play solution
for anyone already using Org-mode for their personal wiki.

Org-roam aims to implement the core features of Roam, leveraging the
mature ecosystem around Org-mode where possible. Eventually, we hope
to further introduce features enabled by the Emacs ecosystem.

[@technovangelist](https://github.com/technovangelist/) has produced a video
describing Org-roam and the concepts behind it:

[![Making Connections in your Notes](http://img.youtube.com/vi/Lg61ocfxk3c/0.jpg)](http://www.youtube.com/watch?v=Lg61ocfxk3c "Making Connections in your Notes")

Important links:

- **[Documentation][docs]**
- **[Discourse][discourse]**
- **[Slack][slack]**

## A Preview

Here's a screencast of Org-roam. The `org-roam-buffer` (window on the
right) shows backlinks for the active Org-roam buffer (window on the
left), as well as the surrounding content in the backlink file. The
database is built once, and updated incrementally. The graph is
generated from the link structure, and can be used to navigate to the
respective files.

![img](doc/images/org-roam-graph.gif)

## Installation

You can install `org-roam` using `package.el`:

```
M-x package-install RET org-roam RET
```

Here's a sample configuration with using `use-package`:

```emacs-lisp
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
```

`org-roam-graph` by default expects to find the `dot` executable
from the `graphviz` package in the `exec-path`.
Ensure `graphviz` is installed and found if you want to use this
feature or customize your configuration for `org-roam-graph` to use a
different tool.

For more detailed installation and configuration instructions (including for
Doom and Spacemacs users), please see [the
documentation][docs].

## Frequently-asked Questions

Q: How do I create a note whose title already matches one of the candidates (e.g. creating `bar` when `barricade` already exists)?

A: With `ivy`, you need to press `C-M-j` to use the current input instead of the nearest candidate. (Source: [`ivy`’s
  FAQ](https://github.com/abo-abo/swiper#frequently-asked-questions))

## Getting Help

Before creating a new topic/issue, please be mindful of our time and ensure
that it has not already been addressed on
[GitHub][issues] or on
[Discourse][discourse].

- If you are new to Emacs and have problem setting up Org-roam, please ask your question on [Slack, channel #how-do-i][slack].
- For quick questions, please ask them on [Slack, channel #troubleshooting][slack].
- If something is not working as it should, or if you would like to suggest a new feature, please [create a new issue][issues].
- If you have questions about your workflow with the slip-box method, please find a relevant topic on [Discourse][discourse], or create a new one.

## Knowledge Bases using Org-roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))

## Changelog

A changelog is being maintained [here](CHANGELOG.md)

## Contributing

To report bugs and suggest new feature use the issue tracker. If you
have some code which you would like to be merged, then open a pull
request. Please also see [CONTRIBUTING.md](.github/CONTRIBUTING.md).

## License

Copyright © Jethro Kuan and contributors. Distributed under the GNU
General Public License, Version 3

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://www.orgroam.com/manual/
[discourse]: https://org-roam.discourse.group/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
[issues]: https://github.com/org-roam/org-roam/issues
