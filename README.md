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

You can install `org-roam` using `package.el`:

```
M-x package-install RET org-roam RET
```

Here's a sample configuration with `use-package`:

```emacs-lisp
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "/path/to/org-files/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
```

The `file-truename` function is only necessary when you use symbolic links
inside `org-roam-directory`: Org-roam does not resolve symbolic links.

Org-roam requires sqlite to function. Org-roam optionally uses Graphviz for
graph-related functionality. It is recommended to install PCRE-enabled ripgrep
for better performance and extended functionality.

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
- [Alexey Shmalko](https://braindump.rasen.dev/)

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
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
[issues]: https://github.com/org-roam/org-roam/issues
[faq]: https://www.orgroam.com/manual.html#FAQ
