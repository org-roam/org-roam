The number of configuration options is deliberately kept small, to
keep the Org-roam codebase manageable. However, we attempt to
accommodate as many usage styles as possible.

All of Org-roam's customization options can be viewed via `M-x
customize-group org-roam`.

## Setting the Org-roam Directory

Set `org-roam-directory` to the folder containing all your Org files:

```emacs-lisp
(setq org-roam-directory "/path/to/org/")
```

Every Org file, at any level of nesting, within `/path/to/org/` is
considered part of the Org-roam ecosystem.

### Having More Than One Org-roam Directory

Emacs supports directory-local variables, allowing the value of
`org-roam-directory` to be different in different directories. It does
this by checking for a file named `.dir-locals.el`. 

To add support for multiple directories, override the
`org-roam-directory` variable using directory-local variables. This is
what `.dir-locals.el` may contain:

```emacs-lisp
((nil . ((org-roam-directory . "/path/to/here/"))))
```

All files within that directory will be treated as their own separate
set of Org-roam files. Remember to run `org-roam-db-build-cache` from a
file within that directory, at least once.

## Org-roam Buffer

The Org-roam buffer defaults to popping up from the right. You may
choose to set it to pop up from the left with `(setq
org-roam-buffer-position 'left)`.

The Org-roam buffer name can also be renamed: e.g. `(setq
org-roam-buffer "*my-buffer-name*")`.

The Org-roam buffer width is adjustable via `org-roam-buffer-width`.
The value of `org-roam-buffer-width` set as a percentage of the total
frame width. For example:

```emacs-lisp
(setq org-roam-buffer-width 0.4)
```

Will result in the Org-roam buffer taking up 40% of the screen width.

You can change backlinks appearance in the buffer by customizing
`org-roam-backlink` face (`M-x customize-face org-roam-backlink`).

## Org-roam Links

By default, links are inserted with the title as the link description.
This can make them hard to distinguish from external links. You may
choose add special indicators for Org-roam links by tweaking
`org-roam-link-title-format`, for example:

```emacs-lisp
(setq org-roam-link-title-format "R:%s")
```

If your version of Org is at least `9.2`, you may also choose to
simply style the link differently, by customizing `org-roam-link` face
(`M-x customize-face org-roam-link`).

## Org-roam Files

Org-roam files are created and prefilled using Org-roam's templating
system. The templating system is customizable, and the system is
described in detail in the [Org-roam Template](templating.md) page.

### Encryption

Encryption (via GPG) can be enabled for all new files by setting
`org-roam-encrypt-files` to `t`. When enabled, new files are created
with the `.org.gpg` extension and decryption are handled automatically
by EasyPG. 

Note that Emacs will prompt for a password for encrypted files during
cache updates if it requires reading the encrypted file. To reduce the
number of password prompts, you may wish to cache the password.

## Org-roam Graph Viewer

Org-roam generates an SVG image using
[Graphviz](https://graphviz.org/). To setup graph navigation, see the
[Graph Setup](graph_setup.md) page.

Org-roam tries its best to locate the Graphviz executable from your
`PATH`, but if it fails to do so, you may set it manually:

```
(setq org-roam-graph-executable "/path/to/dot")
```

You may also choose to use `neato` in place of `dot`, which generates a more compact graph layout.

```
(setq org-roam-graph-executable "/path/to/neato")
(setq org-roam-graph-extra-config '(("overlap" . "false")))
```

Org-roam also attempts to use Firefox (located on `PATH`) to view the
SVG, you may choose to set it to any compatible program:

```
(setq org-roam-graph-viewer "/path/to/image-viewer")
```

### Excluding Nodes and Edges
One may want to exclude certain files to declutter the graph. You can do so by setting `org-roam-graph-exclude-matcher`.

```
(setq org-roam-graph-exclude-matcher '("private" "dailies"))
```

This setting excludes all files whose path contain "private" or "dailies".

## Org-roam Completion System

Org-roam offers completion when choosing note titles etc.
The completion system is configurable. The default setting,

```
(setq org-roam-completion-system 'default)
```

uses Emacs' standard `completing-read`. If you prefer [Helm](https://emacs-helm.github.io/helm/), use

```
(setq org-roam-completion-system 'helm)
```

Other options included `'ido`, and `'ivy'`.
