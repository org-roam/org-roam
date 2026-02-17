;;; org-roam.el --- A database abstraction layer for Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2025 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.3.1
;; Package-Requires: ((emacs "26.1") (compat "30.1") (org "9.6") (emacsql "4.1.0") (magit-section "3.0.0") (truename-cache "0"))

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
;; In order for the package to correctly work through your interactive session
;; it's mandatory to add somewhere to your configuration the next form:
;;
;;     (org-roam-db-autosync-mode)
;;
;; The form can be called both, before or after loading the package, which is up
;; to your preferences. If you call this before the package is loaded, then it
;; will automatically load the package.
;;
;; -----------------------------------------------------------------------------
;;
;; This package also comes with a set of officially supported extensions that
;; provide extra features. You can find them in the "extensions/" subdirectory.
;; These extensions are not automatically loaded with `org-roam`, but they still
;; will be lazy-loaded through their own `autoload's.
;;
;; Org-roam also has other extensions that don't come together with this package.
;; Such extensions are distributed as their own packages, while also
;; authored and maintained by different people on distinct repositories. The
;; majority of them can be found at https://github.com/org-roam and MELPA.
;;
;;; Code:

(require 'rx)
(require 'seq)
(require 'cl-lib)

(require 'compat)

(require 'magit-section)

(require 'emacsql)
;; REVIEW: is this require needed?
;; emacsql-sqlite provides a common interface to an emacsql SQLite backend (e.g. emacs-sqlite-builtin)
;; not to be confused with a backend itself named emacsql-sqlite that existed in emacsql < 4.0.
(require 'emacsql-sqlite)

(require 'org)
(require 'org-attach)                   ; To set `org-attach-id-dir'
(require 'org-id)
(require 'ol)
(require 'org-element)
(require 'org-capture)

(eval-when-compile
  (require 'subr-x))

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

(defcustom org-roam-post-node-insert-hook nil
  "Hook run when an Org-roam node is inserted as an Org link.
Each function takes two arguments: the id of the node, and the link description."
  :group 'org-roam
  :type 'hook)

(defcustom org-roam-dir-exclude-regexps
  (let ((literals
         (append (unless (file-name-absolute-p org-attach-id-dir)
                   (list org-attach-id-dir))
                 '("logseq/version-files/"  ; Duplicates of every Org file
                   "logseq/bak/"  ; Duplicates of every Org file
                   ".git/"  ; Many sub-sub-subdirs
                   ".hg/"))))
    (list (rx ".pam/")  ; pamparam (many files inside)
          (rx (or bos "/") (regexp (regexp-opt literals)))))
  "If a directory name matches any of these regexps, skip it.

Like `org-roam-file-exclude-regexp', this applies to the part of its
file-name after `org-roam-directory' \(i.e. the relative name\), but
with a trailing slash.

You can also use `org-roam-file-exclude-regexp' for the same purpose of
preventing recursion into directories - the separation is just helpful
for the performance of `org-roam-db-sync', so the dir regexps are not
also checked against each each file name."
  :group 'org-roam
  :type '(repeat regexp))

(defcustom org-roam-file-extensions '("org")
  "List of file extensions to be included by Org-Roam.
While a file extension different from \".org\" may be used, the
file still needs to be an `org-mode' file, and it is the user's
responsibility to ensure that."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-file-exclude-regexp (list org-attach-id-dir)
  "Files matching this regexp or list of regexps are excluded from Org-roam."
  :type '(choice
          (repeat
           (string :tag "Regular expression matching files to ignore"))
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam)

;;; Library
(defun org-roam-file-p (&optional file)
  "Return t if FILE is an Org-roam file, nil otherwise.
If FILE is not specified, use the current buffer's file-path.

FILE is an Org-roam file if:
- It's located somewhere under `org-roam-directory'
- It has a matching file extension (`org-roam-file-extensions')
- It doesn't match excluded regexp (`org-roam-file-exclude-regexp')"
  (when (or file (buffer-file-name (buffer-base-buffer)))
    (let* ((path (or file (buffer-file-name (buffer-base-buffer))))
           (relative-path (file-relative-name path org-roam-directory))
           (ext (org-roam--file-name-extension path))
           (ext (if (or (string= ext "gpg")
                        (string= ext "age"))
                    (org-roam--file-name-extension (file-name-sans-extension path))
                  ext))
           (org-roam-dir-p (org-roam-descendant-of-p path org-roam-directory))
           (valid-file-ext-p (member ext org-roam-file-extensions))
           (match-exclude-regexp-p
            (cond
             ((not org-roam-file-exclude-regexp) nil)
             ((stringp org-roam-file-exclude-regexp)
              (string-match-p org-roam-file-exclude-regexp relative-path))
             ((listp org-roam-file-exclude-regexp)
              (let (is-match)
                (dolist (exclude-re org-roam-file-exclude-regexp)
                  (setq is-match (or is-match (string-match-p exclude-re relative-path))))
                is-match)))))
      (save-match-data
        (and
         path
         org-roam-dir-p
         valid-file-ext-p
         (not match-exclude-regexp-p))))))

;;;###autoload
(defun org-roam-list-files ()
  "Return a list of all Org-roam files under `org-roam-directory'.
See `org-roam-file-p' for how each file is determined to be as
part of Org-Roam."
  (mapcar #'car (org-roam-directory-files-and-attributes)))

(defun org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is for an Org-roam file.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam-file-p path)))))

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

;;; Package bootstrap
(provide 'org-roam)

(cl-eval-when (load eval)
  (require 'org-roam-compat)
  (require 'org-roam-utils)
  (require 'org-roam-db)
  (require 'org-roam-node)
  (require 'org-roam-id)
  (require 'org-roam-capture)
  (require 'org-roam-mode)
  (require 'org-roam-log)
  (require 'org-roam-migrate))

;;; org-roam.el ends here
