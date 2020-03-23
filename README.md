[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Documentation Status](https://readthedocs.org/projects/org-roam/badge/?version=latest)](https://org-roam.readthedocs.io/en/latest/?badge=latest)
[![GitHub Release](https://img.shields.io/github/v/release/jethrokuan/org-roam)](https://img.shields.io/github/v/release/jethrokuan/org-roam)
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

As of February 2020, it is in a very early stage of development. 

Important links:

- **[Documentation][docs]**
- **[Org-roam Slack][slack]**

## A Preview

Here's a screenshot of `org-roam`. The `org-roam` buffer shows
backlinks for the active org buffer in the left window, as well as the
surrounding content in the backlink file. The database is built once,
and updated incrementally. The graph is generated from the link
structure, and can be used to navigate to the respective files.

![img](doc/images/org-roam-graph.gif)

## Installation

You can install `org-roam` using `package.el`:

```
M-x package-install RET org-roam RET
```

Here's a sample configuration with using `use-package`:

```emacs-lisp
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

For more detailed installation and configuration instructions (including for
Doom and Spacemacs users), please see [the
documentation](https://org-roam.readthedocs.io/en/master/installation/).

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
[docs]: https://org-roam.readthedocs.io/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-clh0g0tx-j8xg1kVxnrWdKt16gmSGPQ
