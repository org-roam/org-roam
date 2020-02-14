A number of packages work well combined with Org-Roam:

## Deft

[Deft][deft] provides a nice interface for browsing and filtering
org-roam notes.

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

The Deft interface can slow down quickly when the number of files get
huge. [Notdeft][notdeft] is a fork of Deft that's uses an external
search engine and indexer.

## Org-journal

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

## Note-taking Add-ons

These are some plugins that make note-taking in Org-mode more
enjoyable.

### Org-download

[Org-download][org-download] lets you screenshot and yank images from
the web into your notes:

![org-download](images/org-download.gif)

```emacs-lisp
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))
```

### mathpix.el

[mathpix.el][mathpix-el] uses [Mathpix's]() API to convert clips into
latex equations:

![mathpix](images/mathpix.gif)

```emacs-lisp
(use-package mathpix.el
  :straight (:host github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))
```

### Org-noter

[deft]: https://jblevins.org/projects/deft/
[notdeft]: https://github.com/hasu/notdeft
[org-download]: https://github.com/abo-abo/org-download
[mathpix-el]: https://github.com/jethrokuan/mathpix.el
