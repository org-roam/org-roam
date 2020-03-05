To ensure that Org-roam remains manageable, the number of
configuration options is deliberately kept small. However, we have
attempted to accommodate as many usage styles as possible.

In this section, we'll go over the main customization options
available to Org-roam. This section is *crucial*. We need to exploit
the flexibility of Emacs, and mould our tools exactly to our liking.

All of Org-roam's customization options can be viewed via `M-x
customize-group org-roam`.

## Setting the Org-roam Directory

Perhaps the single most important variable to set is
`org-roam-directory`. Set `org-roam-directory` to the folder
containing all your Org files:

```emacs-lisp
(setq org-roam-directory "/path/to/org/")
```

Every Org file, at any level of nesting, within `/path/to/org/` is
considered part of the Org-roam ecosystem.

### Having More Than One Org-roam Directory

Emacs supports customizing variables by directory, so that all files
in a directory and subdirectories will have the same custom
settings. It does this by checking for a file named `.dir-locals.el`
in that directory. This file can override the `org-roam-directory`
variable and all files within that directory will be treated as
their own separate set of Org-roam files.

Here is an example `.dir-locals.el` file that would be placed in a
second Org-roam directory.

```emacs-lisp
((nil . ((org-roam-directory . "/path/to/here/"))))
```

Remember to run `org-roam-build-cache` from a file within that
directory, at least once.

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
I have found this to be a good number.

## Org-roam Links

By default, links are inserted with the title as the link description.
This can make them hard to distinguish from external links. If you
wish, you may choose add special indicators for Org-roam links by
tweaking `org-roam-link-title-format`, for example:

```emacs-lisp
(setq org-roam-link-title-format "R:%s")
```

If your version of Org is at least `9.2`, you may also choose to
simply style the link differently, by customizing `org-roam-link-face`
(`M-x customize-face org-roam-link`).

## Org-roam Files

Org-roam files are typically created via Org-roam's templating system.

### Autopopulating Titles

The default workflow uses the title of the Org file in several
commands. The title is specified via the `#+TITLE:` attribute,
typically near the top of the file. The option
`org-roam-autopopulate-title` defaults to `t`. When true, the title
attribute is automatically inserted into the files created via
Org-roam commands. Setting it to `nil` will disable this behaviour.

### Encryption

Encryption (via GPG) can be enabled for all new files by setting
`org-roam-encrypt-files` to `t`. When enabled, new files are created
with the .org.gpg extension and decryption are handled automatically
by EasyPG. Note that this causes Emacs to ask for password when the
cache is built (if you have an encrypted file in `org-roam-directory`)
as well as each time a new file is created. It might be a good idea to
cache the password in order to make this more managable.

## Org-roam Graph Viewer

Org-roam generates an SVG image using
[Graphviz](https://graphviz.org/). To setup graph navigation, see the
[Graph Setup](graph_setup.md) page.

Org-roam tries its best to locate the Graphviz executable from your
PATH, but if it fails to do so, you may set it manually:

```
(setq org-roam-graphviz-executable "/path/to/dot")
```

Org-roam also attempts to use Firefox (located on PATH) to view the
SVG, you may choose to set it to any compatible program:

```
(setq org-roam-graph-viewer "/path/to/image-viewer")
```
