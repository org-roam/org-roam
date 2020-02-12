## Installation

The recommended method is using [use-package][use-package] and
[straight][straight], or a similar package manager.

```
(use-package org-roam
      :after org
      :hook (org-mode . org-roam-mode)
      :straight (:host github :repo "jethrokuan/org-roam")
      :custom
      (org-roam-directory "/path/to/org-files/")
      (org-roam-link-representation 'title) ;; or keep it as 'id
      :bind
      ("C-c n l" . org-roam)      
      ("C-c n t" . org-roam-today)
      ("C-c n f" . org-roam-find-file)
      ("C-c n i" . org-roam-insert)
      ("C-c n g" . org-roam-show-graph))
```

If not using package.el, you can also clone it into your Emacs
directory and add it to your load path:

```
git clone https://github.com/jethrokuan/org-roam/ ~/.emacs.d/elisp/org-roam
```

```
(add-to-list 'load-path "./elisp")
(require 'org-roam)
```

[use-package]: https://github.com/jwiegley/use-package
[straight]: https://github.com/raxod502/straight.el
