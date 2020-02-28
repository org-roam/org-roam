;;; org-roam-db.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.0") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite)

;;; Options

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.

All Org files, at any level of nesting, is considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defconst org-roam-db-filename "org-roam.db"
  "Name of the Org-roam database file.")

(defconst org-roam--db-version 1)
(defconst org-roam--sqlite-available-p
  (with-demoted-errors "Org-roam initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam--db-connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;; Core

(defun org-roam--get-db ()
  "Return the sqlite db file."
  (interactive "P")
  (expand-file-name org-roam-db-filename org-roam-directory))

(defun org-roam--get-db-connection ()
  "Return the database connection, if any."
  (gethash (file-truename org-roam-directory)
           org-roam--db-connection))

(defun org-roam-db ()
  (unless (and (org-roam--get-db-connection)
               (emacsql-live-p (org-roam--get-db-connection)))
    (let* ((db-file (org-roam--get-db))
           (init-db (not (file-exists-p db-file))))
      (make-directory (file-name-directory db-file) t)
      (let ((conn (emacsql-sqlite db-file)))
        (puthash (file-truename org-roam-directory)
                 conn
                 org-roam--db-connection)
        (when init-db
          (org-roam--db-init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam--db-maybe-update conn version)))
          (cond
           ((> version org-roam--db-version)
            (emacsql-close conn)
            (user-error
             "The Org-roam database was created with a newer Org-roam version. %s"
             "You need to update the Org-roam package.")
            ((< version org-roam--db-version)
             (emacsql-close conn)
             (error "BUG: The Org-roam database scheme changed %s"
                    "and there is no upgrade path"))))))))
  (org-roam--get-db-connection))

;;; Api

(defun org-roam-sql (sql &rest args)
  (if  (stringp sql)
      (emacsql (org-roam-db) (apply #'format sql args))
    (apply #'emacsql (org-roam-db) sql args)))

;;; Schemata

(defconst org-roam--db-table-schemata
  '((files
     [(file :unique :primary-key)
      hash
      last-modified
      ])

    (file-links
     [file-from
      file-to
      properties])

    (titles
     [file
      titles])

    (refs
     [ref
      file])))

(defun org-roam--db-init (db)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) org-roam--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" org-roam--db-version))))

(defun org-roam--db-maybe-update (db version)
  (emacsql-with-transaction db
    'ignore
    ;; Do nothing now
    version))

(defun org-roam--db-close ()
  (when (and (org-roam--get-db-connection)
             (emacsql-live-p (org-roam--get-db-connection)))
    (emacsql-close (org-roam--get-db-connection))))


(provide 'org-roam-db)

;;; org-roam-db.el ends here
