## Installation

The recommended method is using [use-package][use-package] and
[straight][straight], or a similar package manager.

```
(use-package org-roam
      :after org
      :hook 
      ((org-mode . org-roam-mode)
       (after-init . org-roam--build-cache-async) ;; optional!
       )
      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
      :custom
      (org-roam-directory "/path/to/org-files/")
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
(use-package org-roam
      :after org
      :load-path "elisp/"
      :hook 
      ((org-mode . org-roam-mode)
       (after-init . org-roam--build-cache-async) ;; optional!
       )
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind
      ("C-c n l" . org-roam)
      ("C-c n t" . org-roam-today)
      ("C-c n f" . org-roam-find-file)
      ("C-c n i" . org-roam-insert)
      ("C-c n g" . org-roam-show-graph))
```

Or without use-package:

```
(add-to-list 'load-path "./elisp")
(require 'org-roam)
```

There are a number of important configuration options, that greatly
affect the Roam workflow. Do look through them at the
[Configuration](configuration.md) page.

[use-package]: https://github.com/jwiegley/use-package
[straight]: https://github.com/raxod502/straight.el
