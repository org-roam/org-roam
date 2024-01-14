# Org-roam [![GitHub Release][release-badge]][release] [![MELPA][melpa-badge]][melpa] [![License GPL 3][gpl3-badge]][gpl3]

<img src="https://www.orgroam.com/img/logo.svg" align="right" alt="Org-roam Logo" width="240">

Org-roam is a plain-text knowledge management system. It brings some of
[Roam's][roamresearch] more powerful features into the [Org-mode][org]
ecosystem.

Org-roam borrows principles from the Zettelkasten method, providing a solution
for non-hierarchical note-taking. It should also work as a plug-and-play
solution for anyone already using Org-mode for their personal wiki.

- **Private and Secure**: Edit your personal wiki completely offline, entirely
  in your control. Encrypt your notes with GPG. Take lasting notes in
  plain-text.
- **Networked Thought**: Connect notes and thoughts together with ease using
  backlinks. Discover surprising and previously unseen connections in your notes
  with the built-in graph visualization.
- **Extensible and Powerful**: Leverage Emacs' fantastic text-editing interface,
  and the mature Emacs and Org-mode ecosystem of packages.
- **Free and Open Source**: Org-roam is licensed under the GNU General Public
  License version 3 or later.

<p align="center">
  <img src="https://www.orgroam.com/img/screenshot.png" alt="Org-roam Screenshot" width="738">
</p>

- **[Documentation][docs]**
- **[Discourse][discourse]**
- **[Slack][slack]**
- **[Frequently Asked Questions][faq]**
- **[Changelog](CHANGELOG.md)**

## Installation

Down below you will find basic installation instructions for how to quickly
install `org-roam` using various environments for various purposes. For more
detailed information, please read the [manual][docs].

### Using `package.el`
<details>
<summary>Toggle instructions</summary>

You can install `org-roam` from [MELPA](https://melpa.org/) or [MELPA
Stable](https://stable.melpa.org/) using `package.el`:

```
M-x package-install RET org-roam RET
```
</details>

### Using `straight.el`
<details>
<summary>Toggle instructions</summary>

Installation from MELPA or MELPA Stable using `straight.el`:

```emacs-lisp
(straight-use-package 'org-roam)
```

Or with `use-package`:

```emacs-lisp
(use-package org-roam
  :straight t
  ...)
```

If you need to install the package directly from the source repository, instead
of from MELPA, the next sample shows how to do so:

```emacs-lisp
(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  ...)
```

If you plan to use your own local fork for the development and contribution, the
next sample will get you there:

```emacs-lisp
(use-package org-roam
  :straight (:local-repo "/path/to/org-roam-fork"
             :files (:defaults "extensions/*")
             :build (:not compile))
  ...)
```
</details>

### Using Doom Emacs
<details>
<summary>Toggle instructions</summary>

Doom's `:lang org` module comes with support for `org-roam`, but it's not
enabled by default. To activate it pass `+roam2` flag to `org` module in your
`$DOOMDIR/init.el` (e.g. `(org +roam2)`), save the file and run `doom sync -u`
in your shell.

To provide better stability, Doom pins the package to a specific commit. If you
need to unpin it *(not recommended doing that, request Doom to bump the package
instead)* use the next in your `packages.el`:

```emacs-lisp
(unpin! org-roam)
```

If for some reasons you want to use a different recipe for `org-roam`, you can
use the next form in your `packages.el` to install the package from a recipe
repository (e.g. MELPA):

```emacs-lisp
(package! org-roam)
```

You can pass `:pin "commit hash"` to pin the package to a specific commit.

With the next sample you can install the package directly from the source
repository:

```emacs-lisp
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"
           :files (:defaults "extensions/*")))
```

And if you plan to use your own local fork for the development or contribution,
the next sample will get you there:

```emacs-lisp
(package! org-roam
  :recipe (:local-repo "/path/to/org-roam-fork"
           :files (:defaults "extensions/*")
           :build (:not compile)))
```
</details>

### Without a package manager
<details>
<summary>Toggle instructions</summary>

To install the package without using a package manager you have the next two
options:

1. Install the package by cloning it with `git` from the source repository.
2. Or install the package by downloading the latest [release
   version](https://github.com/org-roam/org-roam/releases).
   
In both of the cases you will need to ensure that you have all the required
dependencies. These include:

- dash
- f
- s
- org (9.4 is the minimal required version!)
- emacsql
- emacsql-sqlite
- magit-section
- filenotify-recursive

After installing the package, you will need to properly setup `load-path` to the
package:

``` emacs-lisp
(add-to-list 'load-path "/path/to/org-roam/")
(add-to-list 'load-path "/path/to-org-roam/extensions/")
```

After which you should be able to resolve `(require 'org-roam)` call without any
problems.

Org-roam also comes with `.texi` files to integrate with Emacs' built-in Info
system. Read the manual to find more details for how to install them manually.
</details>

## Configuration

Here's a very basic sample for configuration of `org-roam` using `use-package`:

```emacs-lisp
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/path/to/org-files/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
```

Note that the `file-truename` function is only necessary when you use symbolic
link to `org-roam-directory`. Org-roam won't automatically resolve symbolic link
to the directory.
## Getting Started

[David Wilson](https://github.com/daviwil) of [System
Crafters](https://www.youtube.com/c/SystemCrafters) has produced an introductory
video that covers the basic commands:

[![Getting Started with Org Roam - Build a Second Brain in Emacs](https://img.youtube.com/vi/AyhPmypHDEw/0.jpg)](https://www.youtube.com/watch?v=AyhPmypHDEw)

## Getting Help

Before creating a new topic/issue, please be mindful of our time and ensure that
it has not already been addressed on [GitHub][issues] or on
[Discourse][discourse].

- If you are new to Emacs and have problem setting up Org-roam, please ask your
  question on [Slack, channel #how-do-i][slack].
- For quick questions, please ask them on [Slack, channel
  #troubleshooting][slack].
- If something is not working as it should, or if you would like to suggest a
  new feature, please [create a new issue][issues].
- If you have questions about your workflow with the slip-box method, please
  find a relevant topic on [Discourse][discourse], or create a new one.

## Knowledge Bases using Org-roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))
- [Alexey Shmalko](https://www.alexeyshmalko.com/)
- [Sidharth Arya](https://sidhartharya.github.io/braindump/index.html)
- [Martin Edström](https://edstrom.dev/)

## Contributing

To report bugs and suggest new feature use the issue tracker. If you
have some code which you would like to be merged, then open a pull
request. Please also see [CONTRIBUTING.md](.github/CONTRIBUTING.md).

## License

Copyright © Jethro Kuan and contributors. Distributed under the GNU
General Public License, Version 3.

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[gpl3-badge]: https://img.shields.io/badge/license-GPL_3-green.svg
[gpl3]: http://www.gnu.org/licenses/gpl-3.0.txt
[melpa-badge]: https://melpa.org/packages/org-roam-badge.svg
[melpa]: https://melpa.org/#/org-roam
[release-badge]: https://img.shields.io/github/v/release/org-roam/org-roam
[release]: https://github.com/org-roam/org-roam/releases
[docs]: https://www.orgroam.com/manual.html
[discourse]: https://org-roam.discourse.group/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-wuoize1z-x3UyQnQ0WHF0RhuEQ2NLnQ
[issues]: https://github.com/org-roam/org-roam/issues
[faq]: https://www.orgroam.com/manual.html#FAQ
