[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Documentation Status](https://readthedocs.org/projects/org-roam/badge/?version=latest)](https://org-roam.readthedocs.io/en/latest/?badge=latest)
[![GitHub Release](https://img.shields.io/github/v/release/jethrokuan/org-roam)](https://img.shields.io/github/v/release/jethrokuan/org-roam)

## Synopsis

Org-roam is a rudimentary [Roam][roamresearch] replica built around
the all-powerful [Org-mode][org]. 

Like Roam, Org-roam offers a powerful and effortless non-hierarchical
note-taking approach. With Org-roam, notes flow naturally, making
note-taking fun and easy. Org-roam *enables* a note-taking workflow that
is not fluid with vanilla Org-mode (more in [this blog
post](https://blog.jethro.dev/posts/how_to_take_smart_notes_org/)).


The goal of the project is to implement core features of Roam around
Org-mode, and eventually introduce features enabled by the Emacs
ecosystem.

Visit [the documentation
page](https://org-roam.readthedocs.io/en/latest/) for a tutorial and
more links.

As of February 2020, it is in a very early stage of development. 

## A Preview

Here's a screenshot of `org-roam`. The `org-roam` buffer shows
backlinks for the active org buffer in the left window, as well as the
surrounding content in the backlink file. The backlink database is
built asynchronously in the background, and is not noticeable to the
end user. The graph is generated from the link structure, and can be
used to navigate to the respective files.

![img](doc/images/org-roam-graph.gif)

## Installation

The recommended method is using use-package and straight, or a similar package manager.

```emacs-lisp
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

For more detailed installation instructions (including instructions for
Spacemacs users), please see [the installation
documentation](https://org-roam.readthedocs.io/en/develop/installation/).

## Knowledge Bases using Org-Roam

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
