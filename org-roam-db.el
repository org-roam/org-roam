;;; org-roam-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.0"))

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
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'seq)
(require 'org-macs)
(require 'org-roam-macs)

(defvar org-roam-directory)
(defvar org-roam-enable-headline-linking)
(defvar org-roam-verbose)
(defvar org-roam-file-name)

(declare-function org-roam--org-roam-file-p                "org-roam")
(declare-function org-roam--extract-titles                 "org-roam")
(declare-function org-roam--extract-ref                    "org-roam")
(declare-function org-roam--extract-tags                   "org-roam")
(declare-function org-roam--extract-headlines              "org-roam")
(declare-function org-roam--extract-links                  "org-roam")
(declare-function org-roam--list-all-files                 "org-roam")
(declare-function org-roam--path-to-slug                   "org-roam")
(declare-function org-roam--file-name-extension            "org-roam")
(declare-function org-roam-buffer--update-maybe            "org-roam-buffer")

;;;; Options
(defcustom org-roam-db-location nil
  "The full path to file where the Org-roam database is stored.
If this is non-nil, the Org-roam sqlite database is saved here.

It is the user's responsibility to set this correctly, especially
when used with multiple Org-roam instances."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `org-roam-db-build-cache',
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

(defconst org-roam-db--version 7)

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;;; Core Functions
(defun org-roam-db--get ()
  "Return the sqlite db file."
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
      (let ((conn (emacsql-sqlite3 db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (file-truename org-roam-directory)
                 conn
                 org-roam-db--connection)
        (when init-db
          (org-roam-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-db--update-maybe conn version)))
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
      (meta :not-null)])

    (headlines
     [(id :unique :primary-key)
      (file :not-null)])

    (links
     [(from :not-null)
      (to :not-null)
      (type :not-null)
      (properties :not-null)])

    (tags
     [(file :unique :primary-key)
      (tags)])

    (titles
     [(file :not-null)
      title])

    (refs
     [(ref :unique :not-null)
      (file :not-null)
      (type :not-null)])))

(defun org-roam-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) org-roam-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" org-roam-db--version))))

(defun org-roam-db--update-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version org-roam-db--version)
        (progn
          (org-roam-message (format "Upgrading the Org-roam database from version %d to version %d"
                                    version org-roam-db--version))
          (org-roam-db-build-cache t))))
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
;;;;; Initialization
(defun org-roam-db--initialized-p ()
  "Whether the Org-roam cache has been initialized."
  (and (file-exists-p (org-roam-db--get))
       (> (caar (org-roam-db-query [:select (funcall count) :from titles]))
          0)))

(defun org-roam-db--ensure-built ()
  "Ensures that Org-roam cache is built."
  (unless (org-roam-db--initialized-p)
    (error "[Org-roam] your cache isn't built yet! Please run org-roam-db-build-cache")))

;;;;; Clearing
(defun org-roam-db-clear ()
  "Clears all entries in the Org-roam cache."
  (interactive)
  (when (file-exists-p (org-roam-db--get))
    (dolist (table (mapcar #'car org-roam-db--table-schemata))
      (org-roam-db-query `[:delete :from ,table]))))

(defun org-roam-db--clear-file (&optional filepath)
  "Remove any related links to the file at FILEPATH.
This is equivalent to removing the node from the graph."
  (let ((file (file-truename (or filepath
                                 (buffer-file-name (buffer-base-buffer))))))
    (dolist (table (mapcar #'car org-roam-db--table-schemata))
      (org-roam-db-query `[:delete :from ,table
                           :where (= ,(if (eq table 'links) 'from 'file) $s1)]
                         file))))

;;;;; Insertion
(defun org-roam-db--insert-meta (file hash meta)
  "Insert HASH and META for a FILE into the Org-roam cache."
  (org-roam-db-query
   [:insert :into files
    :values $v1]
   (list (vector file hash meta))))

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
   (mapcar (lambda (title)
             (vector file title)) titles)))

(defun org-roam-db--insert-headlines (headlines)
  "Insert HEADLINES into the Org-roam cache.
Returns t if the insertion was successful, nil otherwise.
Insertions can fail when there is an ID conflict."
  (condition-case nil
      (progn
        (org-roam-db-query
           [:insert :into headlines
            :values $v1]
           headlines)
        t)
    (error
     (unless (listp headlines)
       (setq headlines (list headlines)))
     (lwarn '(org-roam) :error
            (format "Duplicate IDs in %s, one of:\n\n%s\n\nskipping..."
                    (aref (car headlines) 1)
                    (string-join (mapcar (lambda (hl)
                                           (aref hl 0)) headlines) "\n")))
     nil)))

(defun org-roam-db--insert-tags (file tags)
  "Insert TAGS for a FILE into the Org-roam cache."
  (org-roam-db-query
   [:insert :into tags
    :values $v1]
   (list (vector file tags))))

(defun org-roam-db--insert-ref (file ref)
  "Insert REF for FILE into the Org-roam cache.
Returns t if successful, and nil otherwise.
Insertions can fail if the key is already in the database."
  (let ((key (cdr ref))
        (type (car ref)))
    (condition-case nil
        (progn
         (org-roam-db-query
          [:insert :into refs :values $v1]
          (list (vector key file type)))
         t)
      (error
       (lwarn '(org-roam) :error
              (format "Duplicate ref %s in:\n\nA: %s\nB: %s\n\nskipping..."
                      key
                      file
                      (caar (org-roam-db-query
                             [:select file :from refs
                              :where (= ref $v1)]
                             (vector key)))))
       nil))))

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
  (caar (org-roam-db-query [:select [title] :from titles
                            :where (= file $s1)
                            :limit 1]
                           file)))

(defun org-roam-db--get-tags ()
  "Return all distinct tags from the cache."
  (let ((rows (org-roam-db-query [:select :distinct [tags] :from tags]))
        acc)
    (dolist (row rows)
      (dolist (tag (car row))
        (unless (member tag acc)
          (push tag acc))))
    acc))

(defun org-roam-db--connected-component (file)
  "Return all files reachable from/connected to FILE, including the file itself.
If the file does not have any connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"to\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"from\", \"to\" FROM filelinks UNION
                      SELECT \"to\", \"from\" FROM filelinks UNION
                      SELECT \"file\", \"from\" FROM citelinks UNION
                      SELECT \"from\", \"file\" FROM citelinks),
                   connected_component(file) AS
                     (SELECT link FROM links_of WHERE file = $s1
                      UNION
                      SELECT link FROM links_of JOIN connected_component USING(file))
                   SELECT * FROM connected_component;")
         (files (mapcar 'car-safe (emacsql (org-roam-db) query file))))
    files))

(defun org-roam-db--links-with-max-distance (file max-distance)
  "Return all files connected to FILE in at most MAX-DISTANCE steps.
This includes the file itself. If the file does not have any
connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"to\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"from\", \"to\" FROM filelinks UNION
                      SELECT \"to\", \"from\" FROM filelinks UNION
                      SELECT \"file\", \"from\" FROM citelinks UNION
                      SELECT \"from\", \"file\" FROM citelinks),
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

(defun org-roam-db--file-hash (&optional file-path)
  "Compute the hash of FILE-PATH, a file or current buffer."
  (let* ((file-p (and file-path))
         (file-path (or file-path
                        (buffer-file-name (current-buffer))))
         (encrypted-p (and file-path
                           (string= (org-roam--file-name-extension file-path)
                                    "gpg"))))
    (cond ((and encrypted-p file-p)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file-path)
             (secure-hash 'sha1 (current-buffer))))
          (file-p
           (with-temp-buffer
             (insert-file-contents file-path)
             (secure-hash 'sha1 (current-buffer))))
          (t
           (secure-hash 'sha1 (current-buffer))))))

;;;;; Updating
(defun org-roam-db--update-meta ()
  "Update the metadata of the current buffer into the cache."
  (let* ((file (file-truename (buffer-file-name)))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (org-roam-db--file-hash)))
    (org-roam-db-query [:delete :from files
                        :where (= file $s1)]
                       file)
    (org-roam-db--insert-meta file hash (list :atime atime :mtime mtime))))

(defun org-roam-db--update-titles ()
  "Update the title of the current buffer into the cache."
  (let* ((file (file-truename (buffer-file-name)))
         (titles (or (org-roam--extract-titles)
                     (list (org-roam--path-to-slug file)))))
    (org-roam-db-query [:delete :from titles
                        :where (= file $s1)]
                       file)
    (org-roam-db--insert-titles file titles)))

(defun org-roam-db--update-tags ()
  "Update the tags of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name)))
        (tags (org-roam--extract-tags)))
    (org-roam-db-query [:delete :from tags
                        :where (= file $s1)]
                       file)
    (when tags
      (org-roam-db--insert-tags file tags))))

(defun org-roam-db--update-refs ()
  "Update the ref of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from refs
                        :where (= file $s1)]
                       file)
    (when-let ((ref (org-roam--extract-ref)))
      (org-roam-db--insert-ref file ref))))

(defun org-roam-db--update-links ()
  "Update the file links of the current buffer in the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from links
                        :where (= from $s1)]
                       file)
    (when-let ((links (org-roam--extract-links)))
      (org-roam-db--insert-links links))))

(defun org-roam-db--update-headlines ()
  "Update the file headlines of the current buffer into the cache."
  (let* ((file (file-truename (buffer-file-name))))
    (org-roam-db-query [:delete :from headlines
                        :where (= file $s1)]
                       file)
    (when-let ((headlines (org-roam--extract-headlines)))
      (org-roam-db--insert-headlines headlines))))

(defun org-roam-db--update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH."
  (when (org-roam--org-roam-file-p file-path)
    (let ((buf (or (and file-path
                        (find-file-noselect file-path t))
                   (current-buffer))))
      (with-current-buffer buf
        (org-with-wide-buffer
         (emacsql-with-transaction (org-roam-db)
           (org-roam-db--update-meta)
           (org-roam-db--update-tags)
           (org-roam-db--update-titles)
           (org-roam-db--update-refs)
           (when org-roam-enable-headline-linking
             (org-roam-db--update-headlines))
           (org-roam-db--update-links)))
        (org-roam-buffer--update-maybe :redisplay t)))))

(defun org-roam-db-build-cache (&optional force)
  "Build the cache for `org-roam-directory'.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file (org-roam-db--get)))
  (org-roam-db--close) ;; Force a reconnect
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-roam-files (org-roam--list-all-files))
         (current-files (org-roam-db--get-current-files))
         (file-count 0)
         (headline-count 0)
         (link-count 0)
         (tag-count 0)
         (title-count 0)
         (ref-count 0)
         (deleted-count 0))
    (emacsql-with-transaction (org-roam-db)
      ;; Two-step building
      ;; First step: Rebuild files and headlines
      (dolist (file org-roam-files)
        (let* ((attr (file-attributes file))
               (atime (file-attribute-access-time attr))
               (mtime (file-attribute-modification-time attr)))
          (let ((contents-hash (org-roam-db--file-hash file)))
            (unless (string= (gethash file current-files)
                             contents-hash)
              (condition-case nil
                  (org-roam--with-temp-buffer file
                    (org-roam-db--clear-file file)
                    (org-roam-db-query
                     [:insert :into files
                      :values $v1]
                     (vector file contents-hash (list :atime atime :mtime mtime)))
                    (setq file-count (1+ file-count))
                    (when org-roam-enable-headline-linking
                      (when-let ((headlines (org-roam--extract-headlines file)))
                        (when (org-roam-db--insert-headlines headlines)
                          (setq headline-count (1+ headline-count))))))
                (file-error
                 (setq org-roam-files (remove file org-roam-files))
                 (org-roam-db--clear-file file)
                 (lwarn '(org-roam) :warning
                        "Skipping unreadable file while building cache: %s" file)))))))
      ;; Second step: Rebuild the rest
      (dolist (file org-roam-files)
        (let ((contents-hash (org-roam-db--file-hash file)))
          (unless (string= (gethash file current-files)
                           contents-hash)
            (org-roam--with-temp-buffer file
             (when-let (links (org-roam--extract-links file))
               (org-roam-db-query
                [:insert :into links
                 :values $v1]
                links)
               (setq link-count (1+ link-count)))
             (when-let (tags (org-roam--extract-tags file))
               (org-roam-db-query
                [:insert :into tags
                 :values $v1]
                (vector file tags))
               (setq tag-count (1+ tag-count)))
             (let ((titles (or (org-roam--extract-titles)
                               (list (org-roam--path-to-slug file)))))
               (org-roam-db--insert-titles file titles)
               (setq title-count (+ title-count (length titles))))
             (when-let* ((ref (org-roam--extract-ref)))
               (when (org-roam-db--insert-ref file ref)
                 (setq ref-count (1+ ref-count))))))
          (remhash file current-files)))
      (dolist (file (hash-table-keys current-files))
        ;; These files are no longer around, remove from cache...
        (org-roam-db--clear-file file)
        (setq deleted-count (1+ deleted-count))))
    (org-roam-message "files: Δ%s, headlines: Δ%s, links: Δ%s, tags: Δ%s, titles: Δ%s, refs: Δ%s, deleted: Δ%s"
                      file-count
                      headline-count
                      link-count
                      tag-count
                      title-count
                      ref-count
                      deleted-count)))

(provide 'org-roam-db)

;;; org-roam-db.el ends here
