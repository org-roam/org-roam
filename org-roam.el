;;; org-roam.el --- A database abstraction layer for Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "2.90.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Org-roam is a Roam Research inspired Emacs package and is an addition to
;; Org-mode to have a way to quickly process complex SQL-like queries over a
;; large set of plain text Org-mode files. To achieve this Org-roam provides a
;; database abstraction layer, the capabilities of which include, but are not
;; limited to:
;;
;; - Link graph traversal and visualization.
;; - Instantaneous SQL-like queries on headlines
;;   - What are my TODOs, scheduled for X, or due by Y?
;; - Accessing the properties of a node, such as its tags, refs, TODO state or
;;   priority.
;;
;; All of these functionality is powered by this layer. Hence, at its core
;; Org-roam's primary goal is to provide a resilient dual representation of
;; what's already available in plain text, while cached in a binary database,
;; that is cheap to maintain, easy to understand, and is as up-to-date as it
;; possibly can. For users who would like to perform arbitrary programmatic
;; queries on their Org files Org-roam also exposes an API to this database
;; abstraction layer.
;;
;; -----------------------------------------------------------------------------
;;
;; In order for the package to correctly work it's mandatory to add somewhere to
;; your configuration the next form:
;;
;;     (org-roam-setup)
;;
;; The form can be called both, before or after loading the package, which is up
;; to your preferences. If you call this before the package is loaded, then it
;; will automatically load the package.
;;
;; -----------------------------------------------------------------------------
;;
;; This package also comes with a set of officially supported extensions that
;; provide extra features. You can find them in the "extensions/" subdirectory.
;; When `org-roam` is loaded, they are not loaded along, but they still will be
;; lazy-loaded through their own `autoload's.
;;
;; Org-roam also has other extensions that don't come together with the package
;; itself. Such extensions are distributed as their own packages, while also
;; authored and maintained by different people on distinct repositories. The
;; majority of them can be found at https://github.com/org-roam and MELPA.
;;
;;; Code:
(require 'f)
(require 'dash)

(require 'rx)
(require 'seq)
(require 'cl-lib)

(require 'magit-section)

(require 'emacsql)
(require 'emacsql-sqlite)

(require 'org)
(require 'org-id)
(require 'ol)
(require 'org-element)
(require 'org-capture)

(require 'org-roam-utils)
(require 'org-roam-compat)

;;; Options
(defgroup org-roam nil
  "A database abstraction layer for Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-find-file-hook nil
  "Hook run when an Org-roam file is visited."
  :group 'org-roam
  :type 'hook)

(defcustom org-roam-file-extensions '("org")
  "Detected file extensions to include in the Org-roam ecosystem.
The first item in the list is used as the default file extension.
While the file extensions may be different, the file format needs
to be an `org-mode' file, and it is the user's responsibility to
ensure that."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-file-exclude-regexp nil
  "Files matching this regular expression are excluded from the Org-roam."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam)

(defcustom org-roam-list-files-commands
  (if (member system-type '(windows-nt ms-dos cygwin))
      nil
    '(find fd fdfind rg))
  "Commands that will be used to find Org-roam files.

It should be a list of symbols or cons cells representing any of the following
supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `find'
    Use find as the file search method.
    Example command:
    find /path/to/dir -type f \( -name \"*.org\" -o -name \"*.org.gpg\" \)

  `fd'
    Use fd as the file search method.
    Example command: fd /path/to/dir/ --type file -e \".org\" -e \".org.gpg\"

  `fdfind'
    Same as `fd'. It's an alias that used in some OSes (e.g. Debian, Ubuntu)

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "find" find)
              (const :tag "rg" rg)))

;;; Session watcher
;;;###autoload
(defun org-roam-setup ()
  "Setup Org-roam and initialize its database.
This will install the needed hooks and advices to keep everything
in sync with the connected databases."
  (interactive)
  (add-hook 'find-file-hook #'org-roam--file-setup-h)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-add 'rename-file :after #'org-roam--rename-file-a)
  (advice-add 'delete-file :before #'org-roam--delete-file-a)
  (org-roam-db-sync))

(defun org-roam-teardown ()
  "Teardown Org-roam to completely disable it.
This will remove all the hooks and advices installed by
`org-roam-setup' and close all the database connections made by
Org-roam."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam--file-setup-h)
  (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-remove 'rename-file #'org-roam--rename-file-a)
  (advice-remove 'delete-file #'org-roam--delete-file-a)
  (org-roam-db--close-all)
  ;; Disable local hooks for all org-roam buffers
  (dolist (buf (org-roam-buffer-list))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-db--try-update-on-save-h t))))

(defun org-roam--file-setup-h ()
  "Setup an Org-roam file."
  (when (org-roam-file-p)
    (run-hooks 'org-roam-find-file-hook)))

(defun org-roam--delete-file-a (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-file-p file))
    (org-roam-db-clear-file (expand-file-name file))))

(defun org-roam--rename-file-a (old-file new-file-or-dir &rest _args)
  "Maintain cache consistency of file rename.
OLD-FILE is cleared from the database, and NEW-FILE-OR-DIR is added."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam-file-p old-file))
      (org-roam-db-clear-file old-file))
    (when (org-roam-file-p new-file)
      (org-roam-db-update-file new-file))))

;;; Package bootstrap
(provide 'org-roam)

(cl-eval-when (load eval)
  (require 'org-roam-db)
  (require 'org-roam-node)
  (require 'org-roam-capture)
  (require 'org-roam-mode)
  (require 'org-roam-migrate))

;;; org-roam.el ends here
