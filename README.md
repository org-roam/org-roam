<h1 align="center">
<br>
<a href="https://www.orgroam.com/"><img src="https://raw.githubusercontent.com/org-roam/org-roam/369753c98b5612843948809a638cb9997b2599e0/doc/img/logo.svg" alt="Org-roam Logo" width="200"></a>
<br>
Org-roam
<br>
</h1>

<h4 align="center">A plain-text personal knowledge management system. </h4>

<p align="center">
<a href="https://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3"></a>
<a href="https://github.com/org-roam/org-roam/releases"><img src="https://img.shields.io/github/v/release/org-roam/org-roam" alt="GitHub Releases"></a>
<a href="https://melpa.org/#/org-roam"><img src="https://melpa.org/packages/org-roam-badge.svg" alt="MELPA"></a>
</p>

<p align="center">

<a href="#synopsis">Synopsis</a> •
<a href="#preview">Preview</a> •
<a href="#installation">Installation</a> •
<a href="#frequently-asked-questions">FAQ</a> •
<a href="#getting-help">Getting Help</a> •
<a href="#knowledge-bases-using-org-roam">Example Knowledge Bases</a> •
<a href="#changelog">Changelog</a> •
<a href="#contributing">Contributing</a> •
<a href="#license">License</a>
</p>




## Synopsis

> **NOTE:** Org-roam builds upon Emacs and Org-mode, both of which are intricate
> tools that require time investment for mastery. This makes Org-roam less
> friendly for beginners, but extremely powerful for those familiar with the
> ecosystem, or willing to invest effort in it.

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

## Preview

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
               ("C-c n g" . org-roam-graph))
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

## Frequently Asked Questions

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

A changelog is being maintained [here](CHANGELOG.md).

## Contributing

To report bugs and suggest new feature use the issue tracker. If you
have some code which you would like to be merged, then open a pull
request. Please also see [CONTRIBUTING.md](.github/CONTRIBUTING.md).

## License

Copyright © Jethro Kuan and contributors. Distributed under the GNU
General Public License, Version 3.

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://www.orgroam.com/manual/
[discourse]: https://org-roam.discourse.group/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
[issues]: https://github.com/org-roam/org-roam/issues
