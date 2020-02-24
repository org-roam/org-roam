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

You may also choose to simply style the link differently, by
customizing `org-roam-link-face` (`M-x customize-face org-roam-link`).

## Org-roam Files

These customization options revolve around the Org files created and
managed by Org-roam.

### Automatically Creating Files Using Timestamp

A common hassle is ensuring that files are uniquely named within the
Org-roam directory. Org-roam's default workflow utilizes the title of
Org files in all of its main commands (`org-roam-insert`,
`org-roam-find-file`). Hence, having any unique file name is a decent
option, and the default workflow uses the timestamp as the filename.

Org-roam provides templating functionality via `org-roam-templates`.
`org-roam-templates` maps a template string key to a template. Each
template consists of two parts: (1) a function that takes the title,
and generates a filename. (2) the template content. The templated
content accepts two special fields: `${title}` and `${slug}`, which
are substituted with the title and slug respectively. Org-roam ships
with the default template, which inserts the title of the note. 

Here's an example of customizing templates:

```emacs-lisp
(defun jethro/org-roam-title-private (title)
    (let ((timestamp (format-time-string "%Y%m%d%H%M%S" (current-time)))
          (slug (org-roam--title-to-slug title)))
      (format "private-%s_%s" timestamp slug)))
      
(setq org-roam-templates
    (list (list "default" (list :file #'org-roam--file-name-timestamp-title
                                :content "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}"))
          (list "private" (list :file #'jethro/org-roam-title-private
                                :content "#+TITLE: ${title}"))))
```

Here, I define a file-name function `jethro/org-roam-title-private`,
which forms titles like `private-20200202000000-note_name`. The
content string is simply the title. For the default template, I have
extended it to include more boilerplate content for publishing
purposes.

If you wish to be prompted to change the file name on creation, set
`org-roam-filename-noconfirm` to `nil`:

```emacs-lisp
(setq org-roam-filename-noconfirm nil)
```

It is then the user's responsibility to ensure that the file names are
unique.

If you prefer just the title slug as the filename (with no timestamp),
you can use the following template:

```emacs-lisp
(defun my-org-roam-no-timestamp-in-title (title)
    (let ((slug (org-roam--title-to-slug title)))
      (format "%s" slug)))

(setq org-roam-templates
    (list (list "default" (list :file #'my-org-roam-no-timestamp-in-title
:content "#+TITLE: ${title}"))))
````

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
