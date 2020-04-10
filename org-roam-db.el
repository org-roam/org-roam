;;; org-roam-db.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.0.0-rc1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

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
;; This library is provides the underlying database api to org-roam
;;
;;; Code:
;;;; Library Requires
(require 'subr-x)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'org-roam-macs)

(defvar org-roam-directory)
(defvar org-roam-verbose)

(declare-function org-roam--extract-titles      "org-roam")
(declare-function org-roam--extract-ref         "org-roam")
(declare-function org-roam--extract-links       "org-roam")
(declare-function org-roam--list-files          "org-roam")
(declare-function org-roam-buffer--update-maybe "org-roam-buffer")

;;;; Options
(defcustom org-roam-db-location nil
  "Location of the Org-roam database.
If this is non-nil, the Org-roam sqlite database is saved here.

It is the user's responsibility to set this correctly, especially
when used with multiple Org-roam instances."
  :type 'string
  :group 'org-roam)

(defconst org-roam-db--version 2)
(defconst org-roam-db--sqlite-available-p
  (with-demoted-errors "Org-roam initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;;; Core Functions
(defun org-roam-db--get ()
  "Return the sqlite db file."
  (interactive "P")
  (or org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory)))

(defun org-roam-db--get-connection ()
  "Return the database connection, if any."
  (gethash (file-truename org-roam-directory)
           org-roam-db--connection))

(defun org-roam-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam-db--get-connection)
               (emacsql-live-p (org-roam-db--get-connection)))
    (let* ((db-file (org-roam-db--get))
           (init-db (not (file-exists-p db-file))))
      (make-directory (file-name-directory db-file) t)
      (let ((conn (emacsql-sqlite db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (file-truename org-roam-directory)
                 conn
                 org-roam-db--connection)
        (when init-db
          (org-roam-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-db--maybe-update conn version)))
          (cond
           ((> version org-roam-db--version)
            (emacsql-close conn)
            (user-error
             "The Org-roam database was created with a newer Org-roam version.  "
             "You need to update the Org-roam package"))
           ((< version org-roam-db--version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  (org-roam-db--get-connection))

;;;; Entrypoint: (org-roam-db-query)
(defun org-roam-db-query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if  (stringp sql)
      (emacsql (org-roam-db) (apply #'format sql args))
    (apply #'emacsql (org-roam-db) sql args)))

;;;; Schemata
(defconst org-roam-db--table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      (last-modified :not-null)])

    (links
     [(from :not-null)
      (to :not-null)
      (type :not-null)
      (properties :not-null)])

    (titles
     [(file :not-null)
      titles])

    (refs
     [(ref :unique :not-null)
      (file :not-null)])))

(defun org-roam-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) org-roam-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" org-roam-db--version))))

(defun org-roam-db--maybe-update (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (when (= version 1)
      (progn
        (warn "No good way to perform a DB upgrade, rebuilding from scratch...")
        (delete-file (org-roam-db--get))
        (org-roam-db-build-cache)))
    version))

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
;;;;; Initialization
(defun org-roam-db--initialized-p ()
  "Whether the cache has been initialized."
  (and (file-exists-p (org-roam-db--get))
       (> (caar (org-roam-db-query [:select (funcall count) :from titles]))
          0)))

(defun org-roam-db--ensure-built ()
  "Ensures that Org-roam cache is built."
  (unless (org-roam-db--initialized-p)
    (error "[Org-roam] your cache isn't built yet! Please run org-roam-db-build-cache")))

;;;;; Clearing
(defun org-roam-db--clear ()
  "Clears all entries in the caches."
  (interactive)
  (when (file-exists-p (org-roam-db--get))
    (org-roam-db-query [:delete :from files])
    (org-roam-db-query [:delete :from titles])
    (org-roam-db-query [:delete :from links])
    (org-roam-db-query [:delete :from refs])))


(defun org-roam-db--clear-file (&optional filepath)
  "Remove any related links to the file at FILEPATH.
This is equivalent to removing the node from the graph."
  (let* ((path (or filepath
                   (buffer-file-name)))
         (file (file-truename path)))
    (org-roam-db-query [:delete :from files
                        :where (= file $s1)]
                       file)
    (org-roam-db-query [:delete :from links
                        :where (= from $s1)]
                       file)
    (org-roam-db-query [:delete :from titles
                        :where (= file $s1)]
                       file)
    (org-roam-db-query [:delete :from refs
                        :where (= file $s1)]
                       file)))

;;;;; Insertion
(defun org-roam-db--insert-links (links)
  "Insert LINKS into the Org-roam cache."
  (org-roam-db-query
   [:insert :into links
    :values $v1]
   links))

(defun org-roam-db--insert-titles (file titles)
  "Insert TITLES for a FILE into the Org-roam cache."
  (org-roam-db-query
   [:insert :into titles
    :values $v1]
   (list (vector file titles))))

(defun org-roam-db--insert-ref (file ref)
  "Insert REF for FILE into the Org-roam cache."
  (org-roam-db-query
   [:insert :into refs
    :values $v1]
   (list (vector ref file))))

;;;;; Fetching
(defun org-roam-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let* ((current-files (org-roam-db-query [:select * :from files]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-db--get-titles (file)
  "Return the titles of FILE from the cache."
  (caar (org-roam-db-query [:select [titles] :from titles
                            :where (= file $s1)]
                           file
                           :limit 1)))

(defun org-roam-db--connected-component (file)
  "Return all files reachable from/connected to FILE, including the file itself.
If the file does not have any connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (SELECT \"from\", \"to\" FROM links UNION
                      SELECT \"to\", \"from\" FROM links),
                   connected_component(file) AS
                     (SELECT link FROM links_of WHERE file = $s1
                      UNION
                      SELECT link FROM links_of JOIN connected_component USING(file))
                   SELECT * FROM connected_component;")
         (files (mapcar 'car-safe (emacsql (org-roam-db) query file))))
    files))

(defun org-roam-db--links-with-max-distance (file max-distance)
  "Return all files reachable from/connected to FILE in at most MAX-DISTANCE steps,
including the file itself.  If the file does not have any connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (SELECT \"from\", \"to\" FROM links UNION
                      SELECT \"to\", \"from\" FROM links),
                   -- Links are traversed in a breadth-first search.  In order to calculate the
                   -- distance of nodes and to avoid following cyclic links, the visited nodes
                   -- are tracked in 'trace'.
                   connected_component(file, trace) AS
                     (VALUES($s1, json_array($s1))
                      UNION
                      SELECT lo.link, json_insert(cc.trace, '$[' || json_array_length(cc.trace) || ']', lo.link) FROM
                      connected_component AS cc JOIN links_of AS lo USING(file)
                      WHERE (
                        -- Avoid cycles by only visiting each file once.
                        (SELECT count(*) FROM json_each(cc.trace) WHERE json_each.value == lo.link) == 0
                        -- Note: BFS is cut off early here.
                        AND json_array_length(cc.trace) < ($s2 + 1)))
                   SELECT DISTINCT file, min(json_array_length(trace)) AS distance
                   FROM connected_component GROUP BY file ORDER BY distance;")
         ;; In principle the distance would be available in the second column.
         (files (mapcar 'car-safe (emacsql (org-roam-db) query file max-distance))))
    files))

;;;;; Updating
(defun org-roam-db--update-titles ()
  "Update the title of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from titles
                        :where (= file $s1)]
                       file)
    (org-roam-db--insert-titles file (org-roam--extract-titles))))

(defun org-roam-db--update-refs ()
  "Update the ref of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from refs
                        :where (= file $s1)]
                       file)
    (when-let ((ref (org-roam--extract-ref)))
      (org-roam-db--insert-ref file ref))))

(defun org-roam-db--update-cache-links ()
  "Update the file links of the current buffer in the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from links
                        :where (= from $s1)]
                       file)
    (when-let ((links (org-roam--extract-links)))
      (org-roam-db--insert-links links))))

(defun org-roam-db--update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH."
  (let (buf)
    (if file-path
        (setq buf (find-file-noselect file-path))
      (setq buf (current-buffer)))
    (with-current-buffer buf
      (save-excursion
        (org-roam-db--update-titles)
        (org-roam-db--update-refs)
        (org-roam-db--update-cache-links)
        (org-roam-buffer--update-maybe :redisplay t)))))

;;;;; org-roam-db-build-cache
(defun org-roam-db-build-cache ()
  "Build the cache for `org-roam-directory'."
  (interactive)
  (org-roam-db--close) ;; Force a reconnect
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((org-roam-files (org-roam--list-files org-roam-directory))
         (current-files (org-roam-db--get-current-files))
         (time (current-time))
         all-files all-links all-titles all-refs)
    (dolist (file org-roam-files)
      (org-roam--with-temp-buffer
        (insert-file-contents file)
        (let ((contents-hash (secure-hash 'sha1 (current-buffer))))
          (unless (string= (gethash file current-files)
                           contents-hash)
            (org-roam-db--clear-file file)
            (setq all-files
                  (cons (vector file contents-hash time) all-files))
            (when-let (links (org-roam--extract-links file))
              (setq all-links (append links all-links)))
            (let ((titles (org-roam--extract-titles)))
              (setq all-titles (cons (vector file titles) all-titles)))
            (when-let ((ref (org-roam--extract-ref)))
              (setq all-refs (cons (vector ref file) all-refs))))
          (remhash file current-files))))
    (dolist (file (hash-table-keys current-files))
      ;; These files are no longer around, remove from cache...
      (org-roam-db--clear-file file))
    (when all-files
      (org-roam-db-query
       [:insert :into files
        :values $v1]
       all-files))
    (when all-links
      (org-roam-db-query
       [:insert :into links
        :values $v1]
       all-links))
    (when all-titles
      (org-roam-db-query
       [:insert :into titles
        :values $v1]
       all-titles))
    (when all-refs
      (org-roam-db-query
       [:insert :into refs
        :values $v1]
       all-refs))
    (let ((stats (list :files (length all-files)
                       :links (length all-links)
                       :titles (length all-titles)
                       :refs (length all-refs)
                       :deleted (length (hash-table-keys current-files)))))
      (when org-roam-verbose
        (message "files: %s, links: %s, titles: %s, refs: %s, deleted: %s"
                 (plist-get stats :files)
                 (plist-get stats :links)
                 (plist-get stats :titles)
                 (plist-get stats :refs)
                 (plist-get stats :deleted)))
      stats)))

(provide 'org-roam-db)

;;; org-roam-db.el ends here
