To ensure that Org-roam remains manageable, the number of
configuration options is deliberately kept small. However, we have
attempted to accommodate as many usage styles as possible.

In this section, we'll go over the main customization options
available to Org-Roam. This section is *crucial*. We need to exploit
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

## Org-roam Files

These customization options revolve around the Org files created and
managed by Org-roam.

### Automatically Creating Files Using Timestamp

A common hassle is ensuring that files are uniquely named within the
Org-roam directory. Org-roam's default workflow utilizes the title of
Org files in all of its main commands (`org-roam-insert`,
`org-roam-find-file`). Hence, having any unique file name is a decent
option, and the default workflow uses the timestamp as the filename.

The format of the filename is specified by the string
`org-roam-file-format`, which defaults to `"%Y%m%d%H%M%S"`. To see
valid specifications, see the help (`C-h f`) for `format-time-string`.

There are several reasons for keeping filenames meaningful. For
example, one may wish to publish the Org files, and some publishing
methods such as Org-publish use the file names as slugs for the URLs.

If you wish to maintain manual control of filenames, set
`org-roam-use-timestamp-as-filename` to `nil`:

```emacs-lisp
(setq org-roam-use-timestamp-as-filename nil)
```

When this setting is turned off, the user is instead manually prompted
for a filename. It is then the user's responsibility to ensure that
the file names are unique.

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
