;;; org-roam-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

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
;; This library provides the underlying database api to org-roam.
;;
;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'seq)
(require 'cl-lib)

(eval-and-compile
  (require 'org-roam-macs)
  ;; For `org-with-wide-buffer'
  (require 'org-macs))
(require 'org)
(require 'ol)
(require 'org-roam-utils)

(defvar org-roam-find-file-hook)
(defvar org-roam-directory)
(defvar org-roam-verbose)
(defvar org-agenda-files)

(declare-function org-roam-id-at-point "org-roam")
(declare-function org-roam--list-all-files "org-roam")
(declare-function org-roam-node-at-point "org-roam")

;;;; Options
(defcustom org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
  "The full path to file where the Org-roam database is stored.
If this is non-nil, the Org-roam sqlite database is saved here.

It is the user's responsibility to set this correctly, especially
when used with multiple Org-roam instances."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `org-roam-db-sync',
many GC operations happen because of the large number of
temporary structures generated (e.g. parsed ASTs). Temporarily
increasing `gc-cons-threshold' will help reduce the number of GC
operations, at the cost of temporary memory usage.

This defaults to the original value of `gc-cons-threshold', but
tweaking this number may lead to better overall performance. For
example, to reduce the number of GCs, one may set it to a large
value like `most-positive-fixnum'."
  :type 'int
  :group 'org-roam)

(defcustom org-roam-db-node-include-function (lambda () t)
  "A custom function to check if the headline at point is a node."
  :type 'function
  :group 'org-roam)

(defconst org-roam-db-version 16)
(defconst org-roam--sqlite-available-p
  (with-demoted-errors "Org-roam initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;;; Core Functions

(defun org-roam-db--get-connection ()
  "Return the database connection, if any."
  (gethash (expand-file-name org-roam-directory)
           org-roam-db--connection))

(defun org-roam-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam-db--get-connection)
               (emacsql-live-p (org-roam-db--get-connection)))
    (let ((init-db (not (file-exists-p org-roam-db-location))))
      (make-directory (file-name-directory org-roam-db-location) t)
      (let ((conn (emacsql-sqlite org-roam-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name org-roam-directory)
                 conn
                 org-roam-db--connection)
        (when init-db
          (org-roam-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-db--upgrade-maybe conn version)))
          (cond
           ((> version org-roam-db-version)
            (emacsql-close conn)
            (user-error
             "The Org-roam database was created with a newer Org-roam version.  "
             "You need to update the Org-roam package"))
           ((< version org-roam-db-version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  (org-roam-db--get-connection))

;;;; Entrypoint: (org-roam-db-query)
(define-error 'emacsql-constraint "SQL constraint violation")
(defun org-roam-db-query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (apply #'emacsql (org-roam-db) sql args))

(defun org-roam-db-query! (handler sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string.
The query is expected to be able to fail, in this situation, run HANDLER."
  (condition-case err
      (org-roam-db-query sql args)
    (emacsql-constraint
     (funcall handler err))))

;;;; Schemata
(defconst org-roam-db--table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      (atime :not-null)
      (mtime :not-null)])

    (nodes
     ([(id :not-null :primary-key)
       (file :not-null)
       (level :not-null)
       (pos :not-null)
       todo
       priority
       (scheduled text)
       (deadline text)
       title
       properties
       olp]
      (:foreign-key [file] :references files [file] :on-delete :cascade)))

    (aliases
     ([(node-id :not-null)
       alias]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (refs
     ([(node-id :not-null)
       (ref :not-null)
       (type :not-null)]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (tags
     ([(node-id :not-null)
       tag]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (links
     ([(pos :not-null)
       (source :not-null)
       (dest :not-null)
       (type :not-null)
       (properties :not-null)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))

(defconst org-roam-db--table-indices
  '((alias-node-id aliases [node-id])
    (refs-node-id refs [node-id])
    (tags-node-id tags [node-id])))

(defun org-roam-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db "PRAGMA foreign_keys = ON")
    (pcase-dolist (`(,table ,schema) org-roam-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (pcase-dolist (`(,index-name ,table ,columns) org-roam-db--table-indices)
      (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
    (emacsql db (format "PRAGMA user_version = %s" org-roam-db-version))))

(defun org-roam-db--upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version org-roam-db-version)
        (progn
          (org-roam-message (format "Upgrading the Org-roam database from version %d to version %d"
                                    version org-roam-db-version))
          (org-roam-db-sync t))))
  version)

(defun org-roam-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-roam-directory'."
  (unless db
    (setq db (org-roam-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun org-roam-db--close-all ()
  "Closes all database connections made by Org-roam."
  (dolist (conn (hash-table-values org-roam-db--connection))
    (org-roam-db--close conn)))

;;;; Database API
;;;;; Clearing
(defun org-roam-db-clear-all ()
  "Clears all entries in the Org-roam cache."
  (interactive)
  (when (file-exists-p org-roam-db-location)
    (dolist (table (mapcar #'car org-roam-db--table-schemata))
      (org-roam-db-query `[:delete :from ,table]))))

(defun org-roam-db-clear-file (&optional file)
  "Remove any related links to the FILE.
This is equivalent to removing the node from the graph.
If FILE is nil, clear the current buffer."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (org-roam-db-query [:delete :from files
                      :where (= file $s1)]
                     file))

;;;;; Updating tables
(defun org-roam-db-insert-file ()
  "Update the files table for the current buffer.
If UPDATE-P is non-nil, first remove the file in the database."
  (let* ((file (buffer-file-name))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (org-roam-db--file-hash)))
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file hash atime mtime)))))

(defun org-roam-db-get-scheduled-time ()
  "Return the scheduled time at point in ISO8601 format."
  (when-let ((time (org-get-scheduled-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-db-get-deadline-time ()
  "Return the deadline time at point in ISO8601 format."
  (when-let ((time (org-get-deadline-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-db-node-p ()
  "Return t if headline at point is a node, else return nil."
  (and (org-id-get)
       (not (cdr (assoc "ROAM_EXCLUDE" (org-entry-properties))))
       (funcall org-roam-db-node-include-function)))

(defun org-roam-db-map-nodes (fns)
  "Run FNS over all nodes in the current buffer."
  (org-with-point-at 1
    (org-map-entries
     (lambda ()
       (when (org-roam-db-node-p)
         (dolist (fn fns)
           (funcall fn)))))))

(defun org-roam-db-map-links (fns)
  "Run FNS over all links in the current buffer."
  (org-with-point-at 1
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (dolist (fn fns)
          (funcall fn link))))))

(defun org-roam-db-insert-file-node ()
  "Insert the file-level node into the Org-roam cache."
  (org-with-point-at 1
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-link-display-format
                       (or (cadr (assoc "TITLE" (org-collect-keywords '("title"))
                                        #'string-equal))
                           (file-relative-name file org-roam-directory))))
               (pos (point))
               (todo nil)
               (priority nil)
               (scheduled nil)
               (deadline nil)
               (level 0)
               (aliases (org-entry-get (point) "ROAM_ALIASES"))
               (tags org-file-tags)
               (refs (org-entry-get (point) "ROAM_REFS"))
               (properties (org-entry-properties))
               (olp (org-get-outline-path)))
          (org-roam-db-query!
           (lambda (err)
             (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                    (error-message-string err)
                    title id file))
           [:insert :into nodes
            :values $v1]
           (vector id file level pos todo priority
                   scheduled deadline title properties olp))
          (when tags
            (org-roam-db-query
             [:insert :into tags
              :values $v1]
             (mapcar (lambda (tag)
                       (vector id (substring-no-properties tag)))
                     tags)))
          (when aliases
            (org-roam-db-query
             [:insert :into aliases
              :values $v1]
             (mapcar (lambda (alias)
                       (vector id alias))
                     (split-string-and-unquote aliases))))
          (when refs
            (setq refs (split-string-and-unquote refs))
            (let (rows)
              (dolist (ref refs)
                (if (string-match org-link-plain-re ref)
                    (progn
                      (push (vector id (match-string 2 ref)
                                    (match-string 1 ref)) rows))
                  (lwarn '(org-roam) :warning
                         "%s:%s\tInvalid ref %s, skipping..."
                         (buffer-file-name) (point) ref)))
              (when rows
                (org-roam-db-query
                 [:insert :into refs
                  :values $v1]
                 rows)))))))))

(cl-defun org-roam-db-insert-node-data ()
  "Insert node data for headline at point into the Org-roam cache."
  (when-let ((id (org-id-get)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (heading-components (org-heading-components))
           (pos (point))
           (todo (nth 2 heading-components))
           (priority (nth 3 heading-components))
           (level (nth 1 heading-components))
           (scheduled (org-roam-db-get-scheduled-time))
           (deadline (org-roam-db-get-deadline-time))
           (title (or (nth 4 heading-components)
                      (progn (lwarn 'org-roam :warning "Node in %s:%s:%s has no title, skipping..."
                                    file
                                    (line-number-at-pos)
                                    (1+ (- (point) (line-beginning-position))))
                             (cl-return-from org-roam-db-insert-node-data))))
           (properties (org-entry-properties))
           (olp (org-get-outline-path))
           (title (org-link-display-format title)))
      (org-roam-db-query!
       (lambda (err)
         (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                (error-message-string err)
                title id file))
       [:insert :into nodes
        :values $v1]
       (vector id file level pos todo priority
               scheduled deadline title properties olp)))))

(defun org-roam-db-insert-aliases ()
  "Insert aliases for node at point into Org-roam cache."
  (when-let ((node-id (org-id-get))
             (aliases (org-entry-get (point) "ROAM_ALIASES")))
    (org-roam-db-query [:insert :into aliases
                        :values $v1]
                       (mapcar (lambda (alias)
                                 (vector node-id alias))
                               (split-string-and-unquote aliases)))))

(defun org-roam-db-insert-tags ()
  "Insert tags for node at point into Org-roam cache."
  (when-let ((node-id (org-id-get))
             (tags (org-get-tags)))
    (org-roam-db-query [:insert :into tags
                        :values $v1]
                       (mapcar (lambda (tag)
                                 (vector node-id (substring-no-properties tag))) tags))))

(defun org-roam-db-insert-refs ()
  "Insert refs for node at point into Org-roam cache."
  (when-let* ((node-id (org-id-get))
              (refs (org-entry-get (point) "ROAM_REFS"))
              (refs (split-string-and-unquote refs)))
    (let (rows)
      (dolist (ref refs)
        (save-match-data
          (if (string-match org-link-plain-re ref)
              (progn
                (push (vector node-id (match-string 2 ref) (match-string 1 ref)) rows))
            (lwarn '(org-roam) :warning
                   "%s:%s\tInvalid ref %s, skipping..." (buffer-file-name) (point) ref))))
      (org-roam-db-query [:insert :into refs
                          :values $v1]
                         rows))))

(defun org-roam-db-insert-link (link)
  "Insert link data for LINK at current point into the Org-roam cache."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link))
          (properties (list :outline (org-get-outline-path)))
          (source (org-roam-id-at-point)))
      ;; For Org-ref links, we need to split the path into the cite keys
      (when (and (boundp 'org-ref-cite-types)
                 (fboundp 'org-ref-split-and-strip-string)
                 (member type org-ref-cite-types))
        (setq path (org-ref-split-and-strip-string path)))
      (unless (listp path)
        (setq path (list path)))
      (when (and source path)
        (org-roam-db-query
         [:insert :into links
          :values $v1]
         (mapcar (lambda (p)
                   (vector (point) source p type properties))
                 path))))))

;;;;; Fetching
(defun org-roam-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let ((current-files (org-roam-db-query [:select [file hash] :from files]))
        (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-db--file-hash (&optional file-path)
  "Compute the hash of FILE-PATH, a file or current buffer."
  (if file-path
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file-path)
        (secure-hash 'sha1 (current-buffer)))
    (org-with-wide-buffer
     (secure-hash 'sha1 (current-buffer)))))

;;;;; Updating
;;;###autoload (autoload 'org-roam-db-sync "org-roam" nil t)
(defun org-roam-db-sync (&optional force)
  "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file org-roam-db-location))
  (org-roam-db--close) ;; Force a reconnect
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam--list-all-files))
         (current-files (org-roam-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    (emacsql-with-transaction (org-roam-db)
      (if (fboundp 'dolist-with-progress-reporter)
          (dolist-with-progress-reporter (file (hash-table-keys current-files))
              "Clearing removed files..."
            (org-roam-db-clear-file file))
        (dolist (file (hash-table-keys current-files))
          (org-roam-db-clear-file file)))
      (if (fboundp 'dolist-with-progress-reporter)
          (dolist-with-progress-reporter (file modified-files)
              "Processing modified files..."
            (org-roam-db-update-file file))
        (dolist (file modified-files)
          (org-roam-db-update-file file))))))

(defun org-roam-db-update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                           :where (= file $s1)] file-path))))
    (unless (string= content-hash db-hash)
      (org-roam-with-file file-path nil
        (save-excursion
          (org-set-regexps-and-options 'tags-only)
          (org-roam-db-clear-file)
          (org-roam-db-insert-file)
          (org-roam-db-insert-file-node)
          (org-roam-db-map-nodes
           (list #'org-roam-db-insert-node-data
                 #'org-roam-db-insert-aliases
                 #'org-roam-db-insert-tags
                 #'org-roam-db-insert-refs))
          (org-roam-db-map-links
           (list #'org-roam-db-insert-link)))))))

(defun org-roam-db--update-on-save-h ()
  "."
  (add-hook 'after-save-hook #'org-roam-db-update-file nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-db--update-on-save-h)

;; Diagnostic Interactives
(defun org-roam-db-diagnose-node ()
  "Print information about node at point."
  (interactive)
  (prin1 (org-roam-node-at-point)))

(provide 'org-roam-db)

;;; org-roam-db.el ends here
