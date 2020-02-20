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
      ("C-c n b" . org-roam-switch-to-buffer)
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
      ("C-c n b" . org-roam-switch-to-buffer)
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

## Spacemacs
If you are using Spacemacs, you can easily install org-roam by creating a simple layer that wraps org-roam. Paste the following into a new file `/.emacs.d/private/org-roam/packages.el`.
```
(defconst org-roam-packages
  '((org-roam :location
        (recipe :fetcher github :repo "jethrokuan/org-roam" :branch "develop"))))

(defun org-roam/init-org-roam ()
    (use-package org-roam
        :after org
        :hook
        ((org-mode . org-roam-mode)
         (after-init . org-roam--build-cache-async) ;; optional!
         )
        :custom
        (org-roam-directory "/path/to/org-files/")
        :init
        (progn
          (spacemacs/declare-prefix "ar" "org-roam")
          (spacemacs/set-leader-keys
            "arl" 'org-roam
            "art" 'org-roam-today
            "arf" 'org-roam-find-file
            "arg" 'org-roam-show-graph)

          (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
          (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "rl" 'org-roam
            "rt" 'org-roam-today
            "rf" 'org-roam-find-file
            "ri" 'org-roam-insert
            "rg" 'org-roam-show-graph)
          )))
```
Next, append `org-roam` to the `dotspacemacs-configuration-layers` list in your `.spacemacs` configuration file. Reload (`SPC f e R`) or restart Emacs to load `org-roam`. It's functions are available under the prefix `SPC a r` and `, r` when visiting an org-mode buffer. 
