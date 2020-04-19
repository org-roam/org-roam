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
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

Or without `use-package`:

```emacs-lisp
(require 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph)
(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
(org-roam-mode +1)
```

The [Configuration](configuration.md) page details some of the common
configuration options available.

### Completion

Link auto-completion is offered via
[company-org-roam](https://github.com/jethrokuan/company-org-roam/), refer to
the documentation there for further details.

## Spacemacs

If you are using Spacemacs, install org-roam by creating a simple layer that
wraps Org-roam. Paste the following into a new file
`~/.emacs.d/private/org-roam/packages.el`.

```emacs-lisp
(defconst org-roam-packages
  '(org-roam))

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
       "art" 'org-roam-dailies-today
       "arf" 'org-roam-find-file
       "arg" 'org-roam-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
                                                "rl" 'org-roam
                                                "rt" 'org-roam-dailies-today
                                                "rb" 'org-roam-switch-to-buffer
                                                "rf" 'org-roam-find-file
                                                "ri" 'org-roam-insert
                                                "rg" 'org-roam-graph))))
```

Next, append `org-roam` to the `dotspacemacs-configuration-layers`
list in your `.spacemacs` configuration file. Reload (`SPC f e R`) or
restart Emacs to load `org-roam`. It's functions are available under
the prefix `SPC a r` and `, r` when visiting an org-mode buffer.

If you also have the ranger layer installed, the prefix 'ar' conflict
with that of the ranger layer. You might want to change it to 'aor'
(also change the 'ar' to 'aor' in the other key-binding declarations)

## Doom Emacs

[Doom Emacs][doom] has a `+roam` flag on its `org` module for easy
installation and configuration. Simply add the flag to the `org` section
of your `~/.doom.d/init.el` and run `~/.emacs.d/bin/doom sync`.

[use-package]: https://github.com/jwiegley/use-package
[straight]: https://github.com/raxod502/straight.el
[quelpa]: https://github.com/quelpa/quelpa
[doom]: https://github.com/hlissner/doom-emacs
[doom-getting-started]: https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-packages

## Windows

On Windows, if you follow the installation instructions above, you will likely get the error message: **"No EmacSQL SQLite binary available, aborting"**, and `org-roam` won't start properly.

You need to do some additional steps to get `org-roam` to work.

Essentially, you will need to have a binary file for `emacsql-sqlite` so that your Emacs can work with `sqlite` database -- `org-roam` uses it to track backlinks. The following options have been reported to work by Windows users in the community.

Option 1. **Windows Subsystem for Linux (WSL)**
: This option lets you use Linux on your Windows machine. It's Linux, so you don't need to do anything specific for Windows.

Option 2. **mingw-x64**
: Use mingw-x64. You would spend a bit of time to download it, and get familiar with how it works. You should be able to use Linux tools within your Windows [more contribution welcome].

Option 3. **scoop**
: Use [scoop](https://scoop.sh/) to install a couple of software tools (make and gcc) and manually compile a binary (`.exe`) file yourself. Find a short step-by-step guide below.

Option 4. **emacsql-sqlite3**
: Use another Emacs package called [`emacsql-sqlite3`](https://github.com/cireu/emacsql-sqlite3). You can download an [official binary](https://sqlite.org/download.html) for `sqlite3`. `emacsql-sqlite3` lets you use it. For this option to work, you need to adjust the `org-roam` source code, and get your modified version to work in your Emacs environment. Find a suggestion below.

### scoop
**Step 1.** In PowerShell, install `scoop` ([instruction here](https://scoop.sh/)).

```powershell
iwr -useb get.scoop.sh | iex
```

**Step 2.** In PowerShell, install `make` and `gcc` via scoop

```powershell
scoop install make gcc
```

**Step 3.** In Emacs, install the `emacsql-sqlite` package for your Emacs if it is not done yet.

**Step 4.** In PowerShell, move to the directory where `emacsql.c` is stored.

With MELPA, it is likely to be under your ELPA folder:

```
~\AppData\Roaming\.emacs.d\elpa\emacsql-sqlite-20190727.1710\sqlite
```

With Doom Emacs, it should be under your `.emacs\.local`:

```
~\.emacs.d\.local\straight\build\emacsql-sqlite\sqlite
```

Check the files via `dir` command. You should see these files:

```powershell
Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----       22/03/2020  12:10 PM           5170 emacsql.c
-a----       22/03/2020  12:10 PM            439 Makefile
-a----       22/03/2020  12:10 PM        7516138 sqlite3.c
-a----       22/03/2020  12:10 PM         526684 sqlite3.h
```

**Step 5.** Compile the `.exe` file with `make`

```powershell
make emacsql-sqlite CC=gcc LDLIBS=
```

You will see the process triggered with lots of text automatically scrolling down; it may take a couple of minutes for compilation to finish.

Once compilation is done, check that `emacsql-sqlite.exe` has been added to the directory.

**Step 6.** Relaunch Emacs, use `org-roam`

When you start `org-roam` (e.g. via `org-roam-mode`), now you should no longer see the "No EmacSQL SQLite binary available, aborting" error. You are good to go.


### emacsql-sqlite3

1. In Emacs, install the `emacsql-sqlite3` package

2. Using your text editor, etc. modify `org-roam-db.el`:

    1. Replace `(require 'emacsql-sqlite)` with `(require 'emacsql-sqlite3)`

    2. Comment/deactivate the complete `(defconst org-roam-db--sqlite-available-p ... )`

    3. In `(defun org-roam-db ...`, replace `emacsql-sqlite`
with `emacsql-sqlite3`

3. If you compile `.el` files, ensure to replace `org-roam-db.elc` with the new source you modified.
