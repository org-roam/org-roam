## Basic Install and Configuration

Org-roam is now available on MELPA, so you can install it via the following
command:

```
M-x package-install RET org-roam RET
```

Alternatively, you may use package managers such as [straight][straight] or
[quelpa][quelpa] to install the package.

The recommended method of configuration is to use [use-package][use-package].

```emacs-lisp
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

Or without `use-package`:

```emacs-lisp
(require 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-show-graph)
(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
(org-roam-mode +1)
```

The [Configuration](configuration.md) page details some of the common
configuration options available.

## Spacemacs

If you are using Spacemacs, install org-roam by creating a simple layer that
wraps Org-roam. Paste the following into a new file
`~/.emacs.d/private/org-roam/packages.el`.

```emacs-lisp
(defconst org-roam-packages
  '((org-roam :location
              (recipe :fetcher github :repo "jethrokuan/org-roam"))))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
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
                                                "rb" 'org-roam-switch-to-buffer
                                                "rf" 'org-roam-find-file
                                                "ri" 'org-roam-insert
                                                "rg" 'org-roam-show-graph))))
```

Next, append `org-roam` to the `dotspacemacs-configuration-layers`
list in your `.spacemacs` configuration file. Reload (`SPC f e R`) or
restart Emacs to load `org-roam`. It's functions are available under
the prefix `SPC a r` and `, r` when visiting an org-mode buffer.

## Doom Emacs

If you are using [Doom Emacs][doom], configure packages as explained in the
[getting started][doom-getting-started] guide.

Declare Org-roam as a package in your `~/.doom.d/packages.el`:

```elisp
;; ~/.doom.d/packages.el

(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam"))
```

Subsequently, in your `~/.doom.d/config.el` file, configure Org-roam:

```elisp
;; ~/.doom.d/config.el
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init 
  (setq org-roam-directory "/path/to/org-files/")
  (map! :leader 
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (org-roam-mode +1))
```

[use-package]: https://github.com/jwiegley/use-package
[straight]: https://github.com/raxod502/straight.el
[quelpa]: https://github.com/quelpa/quelpa
[doom]: https://github.com/hlissner/doom-emacs
[doom-getting-started]: https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-packages
