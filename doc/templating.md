Rather than creating blank files on `org-roam-insert` and
`org-roam-find-file`, it is may be desirable to prefill the file with
content. This may include:

- Time of creation
- File it was created from
- Clipboard content
- Any other data you may want to input manually

This requires a complex template insertion system, but fortunately,
Org ships with a powerful one: `org-capture`. However, org-capture was
not designed for such use. Org-roam abuses `org-capture` to some
extent, extending its syntax. To first understand how org-roam's
templating system works, it may be useful to look into org-capture.

## Org-roam Templates

The org-roam capture template extends org-capture's template with 2
additional properties:

1. `:file-name`: This is the file name template used when a new note
   is created.
2. `:head`: This is the template that is inserted on initial note
   creation.

### Org-roam Template Expansion

Org-roam's template definitions also extend org-capture's template
syntax, to allow prefilling of strings. In many scenarios,
`org-roam--capture` is passed a mapping between variables and strings.
For example, during `org-roam-insert`, a title is prompted for. If the
title doesn't already exist, we would like to create a new file,
without prompting for the title again.

Variables passed are expanded with the `${var}` syntax. For example,
during `org-roam-insert`, `${title}` is prefilled for expansion. Any
variables that do not contain strings, are prompted for values using
`completing-read`.

After doing this expansion, the org-capture's template expansion
system is used to fill up the rest of the template. You may read up
more on this on [org-capture's documentation
page](https://orgmode.org/manual/Template-expansion.html#Template-expansion).

For example, take the template: `"%<%Y%m%d%H%M%S>-${title}"`, with the title
`"Foo"`. The template is first expanded into `%<%Y%m%d%H%M%S>-Foo`. Then
org-capture expands `%<%Y%m%d%H%M%S>` with timestamp: e.g.
`20200213032037-Foo`.

All of the flexibility afforded by emacs and org-mode are
available. For example, if you want to encode a UTC timestamp in the
filename, you can take advantage of org-mode's `%(EXP)` template
expansion to call `format-time-string` directly to provide its third
argument to specify UTC.

``` emacs-lisp
("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
     :head "#+TITLE: ${title}\n"
     :unnarrowed t)
```

Similarly, if you want to change how titles are transformed into
slugs, you can override `org-roam--title-to-slug`. For example, to use
hyphens instead of underscores:


``` emacs-lisp
  (defun org-roam--title-to-slug (title)
    "Convert TITLE to a filename-suitable slug. Uses hyphens rather than underscores."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("--*" . "-")  ;; remove sequential underscores
                      ("^-" . "")  ;; remove starting underscore
                      ("-$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))
```

This templating system is used throughout org-roam templates.

### Template examples

Here I walkthrough the default template, reproduced below.

```
("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+TITLE: ${title}\n"
     :unnarrowed t)
```

1. The template has short key `"d"`. If you have only one template,
   org-roam automatically chooses this template for you.
2. The template is given a description of `"default"`.
3. `plain` text is inserted. Other options include Org headings via
   `entry`.
4. `(function org-roam--capture-get-point)` should not be changed.
5. `"%?"` is the template inserted on each call to `org-roam--capture`.
   This template means don't insert any content, but place the cursor
   here.
6. `:file-name` is the file-name template for a new note, if it
doesn't yet exist. This creates a file at path that looks like
   `/path/to/org-roam-directory/20200213032037-foo.org`.
7. `:head` contains the initial template to be inserted (once only),
   at the beginning of the file. Here, the title global attribute is
   inserted.
8. `:unnarrowed t` tells org-capture to show the contents for the
   whole file, rather than narrowing to just the entry.

Other options you may want to learn about include `:immediate-finish`.
