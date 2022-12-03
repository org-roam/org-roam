;;; org-roam-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "3.0.0"))

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
;; This module provides the underlying database API to Org-roam.
;;
;;; Code:
(require 'org-roam)
(require 'url-parse)
(require 'ol)
(defvar org-outline-path-cache)

;;; Options
(defcustom org-roam-database-connector 'sqlite
  "The database connector used by Org-roam.
This must be set before `org-roam' is loaded.  To use an alternative
connector you must install the respective package explicitly.
The default is `sqlite', which uses the `emacsql-sqlite' library
that is being maintained in the same repository as `emacsql'
itself.
If you are using Emacs 29, then the recommended connector is
`sqlite-builtin', which uses the new builtin support for SQLite.
You need to install the `emacsql-sqlite-builtin' package to use
this connector.
If you are using an older Emacs release, then the recommended
connector is `sqlite-module', which uses the module provided by
the `sqlite3' package.  This is very similar to the previous
connector and the built-in support in Emacs 29 derives from this
module.  You need to install the `emacsql-sqlite-module' package
to use this connector.
For the time being `libsqlite3' is still supported.  Do not use
this, it is an older version of the `sqlite-module' connector
from before the connector and the package were renamed.
For the time being `sqlite3' is also supported.  Do not use this.
This uses the third-party `emacsql-sqlite3' package, which uses
the official `sqlite3' cli tool, which is not intended
to be used like this.  See https://nullprogram.com/blog/2014/02/06/."
  :package-version '(forge . "0.3.0")
  :group 'org-roam
  :type '(choice (const sqlite)
                 (const sqlite-builtin)
                 (const sqlite-module)
                 (const :tag "libsqlite3 (OBSOLETE)" libsqlite3)
                 (const :tag "sqlite3 (BROKEN)" sqlite3)))

(defcustom org-roam-db-location (locate-user-emacs-file "org-roam.db")
  "The path to file where the Org-roam database is stored.

It is the user's responsibility to set this correctly, especially
when used with multiple Org-roam instances."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During `org-roam-db-sync', Emacs can pause multiple times to
perform garbage collection because of the large number of
temporary structures generated (e.g. parsed ASTs).

`gc-cons-threshold' is temporarily set to
`org-roam-db-gc-threshold' during this operation, and increasing
`gc-cons-threshold' will help reduce the number of GC operations,
at the cost of memory usage. Tweaking this value may lead to
better overall performance.

For example, to reduce the number of GCs to the minimum, on
machines with large memory one may set it to
`most-positive-fixnum'."
  :type 'int
  :group 'org-roam)

(defcustom org-roam-db-node-include-function (lambda () t)
  "A custom function to check if the point contains a valid node.
This function is called each time a node (both file and headline)
is about to be saved into the Org-roam database.

If the function returns nil, Org-roam will skip the node. This
function is useful for excluding certain nodes from the Org-roam
database."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-db-update-on-save t
  "If t, update the Org-roam database upon saving the file.
Disable this if your files are large and updating the database is
slow."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-db-extra-links-elements '(node-property keyword)
  "The list of Org element types to include for parsing by Org-roam.

By default, when parsing Org's AST, links within keywords and
property drawers are not parsed as links. Sometimes however, it
is desirable to parse and cache these links (e.g. hiding links in
a property drawer)."
  :package-version '(org-roam . "2.2.0")
  :group 'org-roam
  :type '(set (const :tag "keywords" keyword)
              (const :tag "property drawers" node-property)))

(defcustom org-roam-db-extra-links-exclude-keys '((node-property . ("ROAM_REFS"))
                                                  (keyword . ("transclude")))
  "Keys to ignore when mapping over links.

The car of the association list is the Org element type (e.g.
keyword). The cdr is a list of case-insensitive strings to
exclude from being treated as links.

For example, we use this to prevent self-referential links in
ROAM_REFS."
  :package-version '(org-roam . "2.2.0")
  :group 'org-roam
  :type '(alist))

;;; Variables
(defconst org-roam-db-version 18)

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;; Core Functions
(defun org-roam-db--get-connection ()
  "Return the database connection, if any."
  (gethash (expand-file-name (file-name-as-directory org-roam-directory))
           org-roam-db--connection))

(declare-function emacsql-sqlite "ext:emacsql-sqlite")
(declare-function emacsql-sqlite3 "ext:emacsql-sqlite3")
(declare-function emacsql-libsqlite3 "ext:emacsql-libsqlite3")
(declare-function emacsql-sqlite-builtin "ext:emacsql-sqlite-builtin")
(declare-function emacsql-sqlite-module "ext:emacsql-sqlite-module")

(defun org-roam-db--conn-fn ()
  "Return the function for creating the database connection."
  (cl-case org-roam-database-connector
    (sqlite
     (progn
       (require 'emacsql-sqlite)
       #'emacsql-sqlite))
    (sqlite-builtin
     (progn
       (require 'emacsql-sqlite-builtin)
       #'emacsql-sqlite-builtin))
    (sqlite-module
     (progn
       (require 'emacsql-sqlite-module)
       #'emacsql-sqlite-module))
    (libsqlite3
     (progn
       (require 'emacsql-libsqlite3)
       #'emacsql-libsqlite3))
    (sqlite3
     (progn
       (require 'emacsql-sqlite3)
       #'emacsql-sqlite3))))

(defun org-roam-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam-db--get-connection)
               (emacsql-live-p (org-roam-db--get-connection)))
    (let ((init-db (not (file-exists-p org-roam-db-location))))
      (make-directory (file-name-directory org-roam-db-location) t)
      (let ((conn (funcall (org-roam-db--conn-fn) org-roam-db-location)))
        (emacsql conn [:pragma (= foreign_keys ON)])
        (when-let* ((process (emacsql-process conn))
                    (_ (processp process)))
          (set-process-query-on-exit-flag process nil))
        (puthash (expand-file-name (file-name-as-directory org-roam-directory))
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

;;; Entrypoint: (org-roam-db-query)
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

;;; Schemata
(defconst org-roam-db--table-schemata
  '((files
     [(file :unique :primary-key)
      title
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

    (citations
     ([(node-id :not-null)
       (cite-key :not-null)
       (pos :not-null)
       properties]
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

;;; Database API
;;;; Clearing
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

;;;; Updating tables

(defun org-roam-db--file-title ()
  "In current Org buffer, get the title.
If there is no title, return the file name relative to
`org-roam-directory'."
  (org-link-display-format
   (or (string-join (cdr (assoc "TITLE" (org-collect-keywords '("title")))) " ")
       (file-name-sans-extension (file-relative-name
                                  (buffer-file-name (buffer-base-buffer))
                                  org-roam-directory)))))

(defun org-roam-db-insert-file (&optional hash)
  "Update the files table for the current buffer.
If UPDATE-P is non-nil, first remove the file in the database.
If HASH is non-nil, use that as the file's hash without recalculating it."
  (let* ((file (buffer-file-name))
         (file-title (org-roam-db--file-title))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (or hash (org-roam-db--file-hash file))))
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file file-title hash atime mtime)))))

(defun org-roam-db-get-scheduled-time ()
  "Return the scheduled time at point in ISO8601 format."
  (when-let ((time (org-get-scheduled-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-db-get-deadline-time ()
  "Return the deadline time at point in ISO8601 format."
  (when-let ((time (org-get-deadline-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-db-node-p ()
  "Return t if headline at point is an Org-roam node, else return nil."
  (and (org-id-get)
       (not (org-entry-get (point) "ROAM_EXCLUDE"))
       (funcall org-roam-db-node-include-function)))

(defun org-roam-db-map-nodes (fns)
  "Run FNS over all nodes in the current buffer."
  (org-with-wide-buffer
   (org-map-region
    (lambda ()
      (when (org-roam-db-node-p)
        (dolist (fn fns)
          (funcall fn))))
    (point-min) (point-max))))

(defun org-roam-db-map-links (fns)
  "Run FNS over all links in the current buffer."
  (org-with-point-at 1
    (while (re-search-forward org-link-any-re nil :no-error)
      ;; `re-search-forward' let the cursor one character after the link, we need to go backward one char to
      ;; make the point be on the link.
      (backward-char)
      (let* ((begin (match-beginning 0))
             (element (org-element-context))
             (type (org-element-type element))
             link bounds)
        (cond
         ;; Links correctly recognized by Org Mode
         ((eq type 'link)
          (setq link element))
         ;; Links in property drawers and lines starting with #+. Recall that, as for Org Mode v9.4.4, the
         ;; org-element-type of links within properties drawers is "node-property" and for lines starting with
         ;; #+ is "keyword".
         ((and (member type org-roam-db-extra-links-elements)
               (not (member-ignore-case (org-element-property :key element)
                                        (cdr (assoc type org-roam-db-extra-links-exclude-keys))))
               (setq link (save-excursion
                            (goto-char begin)
                            (save-match-data (org-element-link-parser)))))))
        (when link
          (dolist (fn fns)
            (funcall fn link)))))))

(defun org-roam-db-map-citations (info fns)
  "Run FNS over all citations in the current buffer.
INFO is the org-element parsed buffer."
  (org-element-map info 'citation-reference
    (lambda (cite)
      (dolist (fn fns)
        (funcall fn cite)))))

(defun org-roam-db-insert-file-node ()
  "Insert the file-level node into the Org-roam cache."
  (org-with-point-at 1
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-roam-db--file-title))
               (pos (point))
               (todo nil)
               (priority nil)
               (scheduled nil)
               (deadline nil)
               (level 0)
               (tags org-file-tags)
               (properties (org-entry-properties))
               (olp nil))
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
          (org-roam-db-insert-aliases)
          (org-roam-db-insert-refs))))))

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
           (olp (org-get-outline-path nil 'use-cache))
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
  (when-let* ((node-id (org-id-get))
              (aliases (org-entry-get (point) "ROAM_ALIASES"))
              (aliases (split-string-and-unquote aliases)))
    (org-roam-db-query [:insert :into aliases
                        :values $v1]
                       (mapcar (lambda (alias)
                                 (vector node-id alias))
                               aliases))))

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
          (cond (;; @citeKey
                 (string-prefix-p "@" ref)
                 (push (vector node-id (substring ref 1) "cite") rows))
                (;; [cite:@citeKey]
                 (string-prefix-p "[cite:" ref)
                 (condition-case nil
                     (let ((cite-obj (org-cite-parse-objects ref)))
                       (org-element-map cite-obj 'citation-reference
                         (lambda (cite)
                           (let ((key (org-element-property :key cite)))
                             (push (vector node-id key "cite") rows)))))
                   (error
                    (lwarn '(org-roam) :warning
                           "%s:%s\tInvalid cite %s, skipping..." (buffer-file-name) (point) ref))))
                (;; https://google.com, cite:citeKey
                 ;; Note: we use string-match here because it matches any link: e.g. [[cite:abc][abc]]
                 ;; But this form of matching is loose, and can accept invalid links e.g. [[cite:abc]
                 (string-match org-link-any-re (org-link-encode ref '(#x20)))
                 (setq ref (org-link-encode ref '(#x20)))
                 (let ((ref-url (url-generic-parse-url (or (match-string 2 ref) (match-string 0 ref))))
                       (link-type ()) ;; clear url-type for backward compatible.
                       (path ()))
                   (setq link-type (url-type ref-url))
                   (setf (url-type ref-url) nil)
                   (setq path (org-link-decode (url-recreate-url ref-url)))
                   (if (and (boundp 'org-ref-cite-types)
                            (or (assoc link-type org-ref-cite-types)
                                (member link-type org-ref-cite-types)))
                       (dolist (key (org-roam-org-ref-path-to-keys path))
                         (push (vector node-id key link-type) rows))
                     (push (vector node-id path link-type) rows))))
                (t
                 (lwarn '(org-roam) :warning
                        "%s:%s\tInvalid ref %s, skipping..." (buffer-file-name) (point) ref)))))
      (when rows
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           rows)))))

(defun org-roam-db-insert-link (link)
  "Insert link data for LINK at current point into the Org-roam cache."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link))
          (source (org-roam-id-at-point))
          (properties (list :outline (ignore-errors
                                       ;; This can error if link is not under any headline
                                       (org-get-outline-path 'with-self 'use-cache)))))
      ;; For Org-ref links, we need to split the path into the cite keys
      (when (and source path)
        (if (and (boundp 'org-ref-cite-types)
                 (or (assoc type org-ref-cite-types)
                     (member type org-ref-cite-types)))
            (org-roam-db-query
             [:insert :into citations
              :values $v1]
             (mapcar (lambda (k) (vector source k (point) properties))
                     (org-roam-org-ref-path-to-keys path)))
          (org-roam-db-query
           [:insert :into links
            :values $v1]
           (vector (point) source path type properties)))))))

(defun org-roam-db-insert-citation (citation)
  "Insert data for CITATION at current point into the Org-roam cache."
  (save-excursion
    (goto-char (org-element-property :begin citation))
    (let ((key (org-element-property :key citation))
          (source (org-roam-id-at-point))
          (properties (list :outline (ignore-errors
                                       ;; This can error if link is not under any headline
                                       (org-get-outline-path 'with-self 'use-cache)))))
      (when (and source key)
        (org-roam-db-query
         [:insert :into citations
          :values $v1]
         (vector source key (point) properties))))))

;;;; Fetching
(defun org-roam-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let ((current-files (org-roam-db-query [:select [file hash] :from files]))
        (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-db--file-hash (file-path)
  "Compute the hash of FILE-PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'sha1 (current-buffer))))

;;;; Synchronization
(defun org-roam-db-update-file (&optional file-path no-require)
  "Update Org-roam cache for FILE-PATH.

If the file does not exist anymore, remove it from the cache.

If the file exists, update the cache with information.

If NO-REQUIRE, don't require optional libraries. Set NO-REQUIRE
when the libraries are already required at some toplevel, e.g.
in `org-roam-db-sync'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                           :where (= file $s1)] file-path)))
        info)
    (unless (string= content-hash db-hash)
      (unless no-require
        (org-roam-require '(org-ref oc)))
      (org-roam-with-file file-path nil
        (emacsql-with-transaction (org-roam-db)
          (org-with-wide-buffer
           (org-set-regexps-and-options 'tags-only)
           (org-refresh-category-properties)
           (org-roam-db-clear-file)
           (org-roam-db-insert-file content-hash)
           (org-roam-db-insert-file-node)
           (setq org-outline-path-cache nil)
           (org-roam-db-map-nodes
            (list #'org-roam-db-insert-node-data
                  #'org-roam-db-insert-aliases
                  #'org-roam-db-insert-tags
                  #'org-roam-db-insert-refs))
           (setq org-outline-path-cache nil)
           (setq info (org-element-parse-buffer))
           (org-roam-db-map-links
            (list #'org-roam-db-insert-link))
           (when (fboundp 'org-cite-insert)
             (require 'oc)             ;ensure feature is loaded
             (org-roam-db-map-citations
              info
              (list #'org-roam-db-insert-citation)))))))))

;;;###autoload
(defun org-roam-db-sync (&optional force)
  "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (org-roam-db--close) ;; Force a reconnect
  (when force (delete-file org-roam-db-location))
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (org-roam-require '(org-ref oc))
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam-list-files))
         (current-files (org-roam-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    (emacsql-with-transaction (org-roam-db)
      (org-roam-dolist-with-progress (file (hash-table-keys current-files))
          "Clearing removed files..."
        (org-roam-db-clear-file file))
      (org-roam-dolist-with-progress (file modified-files)
          "Processing modified files..."
        (condition-case err
            (org-roam-db-update-file file 'no-require)
          (error
           (org-roam-db-clear-file file)
           (lwarn 'org-roam :error "Failed to process %s with error %s, skipping..."
                  file (error-message-string err))))))))

;;;###autoload
(define-minor-mode org-roam-db-autosync-mode
  "Global minor mode to keep your Org-roam session automatically synchronized.
Through the session this will continue to setup your
buffers (that are Org-roam file visiting), keep track of the
related changes, maintain cache consistency and incrementally
update the currently active database.

If you need to manually trigger resync of the currently active
database, see `org-roam-db-sync' command."
  :group 'org-roam
  :global t
  :init-value nil
  (let ((enabled org-roam-db-autosync-mode))
    (cond
     (enabled
      (add-hook 'find-file-hook  #'org-roam-db-autosync--setup-file-h)
      (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
      (advice-add #'rename-file :after  #'org-roam-db-autosync--rename-file-a)
      (advice-add #'delete-file :before #'org-roam-db-autosync--delete-file-a)
      (org-roam-db-sync))
     (t
      (remove-hook 'find-file-hook  #'org-roam-db-autosync--setup-file-h)
      (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
      (advice-remove #'rename-file #'org-roam-db-autosync--rename-file-a)
      (advice-remove #'delete-file #'org-roam-db-autosync--delete-file-a)
      (org-roam-db--close-all)
      ;; Disable local hooks for all org-roam buffers
      (dolist (buf (org-roam-buffer-list))
        (with-current-buffer buf
          (remove-hook 'after-save-hook #'org-roam-db-autosync--try-update-on-save-h t)))))))

;;;###autoload
(defun org-roam-db-autosync-enable ()
  "Activate `org-roam-db-autosync-mode'."
  (org-roam-db-autosync-mode +1))

(defun org-roam-db-autosync-disable ()
  "Deactivate `org-roam-db-autosync-mode'."
  (org-roam-db-autosync-mode -1))

(defun org-roam-db-autosync-toggle ()
  "Toggle `org-roam-db-autosync-mode' enabled/disabled."
  (org-roam-db-autosync-mode 'toggle))

(defun org-roam-db-autosync--delete-file-a (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-file-p file))
    (org-roam-db-clear-file (expand-file-name file))))

(defun org-roam-db-autosync--rename-file-a (old-file new-file-or-dir &rest _args)
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

(defun org-roam-db-autosync--setup-file-h ()
  "Setup the current buffer if it visits an Org-roam file."
  (when (org-roam-file-p) (run-hooks 'org-roam-find-file-hook)))

(add-hook 'org-roam-find-file-hook #'org-roam-db-autosync--setup-update-on-save-h)
(defun org-roam-db-autosync--setup-update-on-save-h ()
  "Setup the current buffer to update the DB after saving the current file."
  (add-hook 'after-save-hook #'org-roam-db-autosync--try-update-on-save-h nil t))

(defun org-roam-db-autosync--try-update-on-save-h ()
  "If appropriate, update the database for the current file after saving buffer."
  (when org-roam-db-update-on-save (org-roam-db-update-file)))

;;; Diagnostics
(defun org-roam-db-diagnose-node ()
  "Print information about node at point."
  (interactive)
  (prin1 (org-roam-node-at-point)))

(provide 'org-roam-db)

;;; org-roam-db.el ends here
