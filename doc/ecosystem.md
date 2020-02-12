## Ecosystem

A number of packages work well combined with Org-Roam:

### Deft
[Deft](https://jblevins.org/projects/deft/) provides a nice
interface for browsing and filtering org-roam notes.

```
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "/path/to/org-roam-files/")
  (deft-use-filename-as-title t))
```

### Org-journal
[Org-journal](https://github.com/bastibe/org-journal) is a more
powerful alternative to the simple function `org-roam-today`. It
provides better journaling capabilities, and a nice calendar interface
to see all dated entries.

```
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "/path/to/org-roam-files/")
  (org-journal-date-format "%A, %d %B %Y"))
```
