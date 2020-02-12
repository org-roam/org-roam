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
org-roam commands. Setting it to `nil` will disable this behaviour.
