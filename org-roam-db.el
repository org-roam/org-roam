;;; org-roam-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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

(eval-and-compile
  (require 'org-roam-macs)
  ;; For `org-with-wide-buffer'
  (require 'org-macs))

(defvar org-roam-directory)
(defvar org-roam-enable-headline-linking)
(defvar org-roam-verbose)
(defvar org-roam-file-name)

(defvar org-agenda-files)

(declare-function org-roam--org-roam-file-p                "org-roam")
(declare-function org-roam--extract-titles                 "org-roam")
(declare-function org-roam--extract-refs                   "org-roam")
(declare-function org-roam--extract-tags                   "org-roam")
(declare-function org-roam--extract-ids                    "org-roam")
(declare-function org-roam--extract-links                  "org-roam")
(declare-function org-roam--list-all-files                 "org-roam")
(declare-function org-roam--path-to-slug                   "org-roam")
(declare-function org-roam--file-name-extension            "org-roam")
(declare-function org-roam-buffer--update-maybe            "org-roam-buffer")

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

(defconst org-roam-db--version 10)

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

(defvar org-roam-db-dirty nil
  "Whether the org-roam database is dirty and requires an update.
Contains pairs of `org-roam-directory' and `org-roam-db-location'
so that multi-directories are updated.")

(defcustom org-roam-db-update-method 'idle-timer
  "Method to update the Org-roam database.

`immediate'
  Update the database immediately upon file changes.

`idle-timer'
  Updates the database if dirty, if Emacs idles for `org-roam-db-update-idle-seconds'."
  :type '(choice (const :tag "idle-timer" idle-timer)
		 (const :tag "immediate" immediate))
  :group 'org-roam)

(defcustom org-roam-db-update-idle-seconds 2
  "Number of idle seconds before triggering an Org-roam database update."
  :type 'integer
  :group 'org-roam)

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
      (let ((conn (emacsql-sqlite3 org-roam-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name org-roam-directory)
                 conn
                 org-roam-db--connection)
        (when init-db
          (org-roam-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-db--upgrade-maybe conn version)))
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

    (ids
     [(id :unique :primary-key)
      (file :not-null)
      (level :not-null)])

    (links
     [(source :not-null)
      (dest :not-null)
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

(defun org-roam-db--upgrade-maybe (db version)
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

;;;; Timer-based updating
(defvar org-roam-db-file-update-timer nil
  "Timer for updating the database when dirty.")

(defun org-roam-db-mark-dirty ()
  "Mark the Org-roam database as dirty."
  (add-to-list 'org-roam-db-dirty (list org-roam-directory org-roam-db-location)
               nil #'equal))

(defun org-roam-db-update-cache-on-timer ()
  "Update the cache if the database is dirty.
This function is called on `org-roam-db-file-update-timer'."
  (pcase-dolist (`(,org-roam-directory ,org-roam-db-location) org-roam-db-dirty)
    (org-roam-db-build-cache))
  (setq org-roam-db-dirty nil))

;;;; Database API
;;;;; Initialization
(defun org-roam-db--initialized-p ()
  "Whether the Org-roam cache has been initialized."
  (and (file-exists-p org-roam-db-location)
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
  (when (file-exists-p org-roam-db-location)
    (dolist (table (mapcar #'car org-roam-db--table-schemata))
      (org-roam-db-query `[:delete :from ,table]))))

(defun org-roam-db--clear-file (&optional file)
  "Remove any related links to the FILE.
This is equivalent to removing the node from the graph."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (dolist (table (mapcar #'car org-roam-db--table-schemata))
    (org-roam-db-query `[:delete :from ,table
                         :where (= ,(if (eq table 'links) 'source 'file) $s1)]
                       file)))

;;;;; Inserting
(defun org-roam-db--insert-meta (&optional update-p)
  "Update the metadata of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the meta for the file in the database."
  (let* ((file (or org-roam-file-name (buffer-file-name)))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (org-roam-db--file-hash)))
    (when update-p
      (org-roam-db-query [:delete :from files
                          :where (= file $s1)]
                         file))
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file hash (list :atime atime :mtime mtime))))))

(defun org-roam-db--insert-titles (&optional update-p)
  "Update the titles of the current buffer into the cache.
If UPDATE-P is non-nil, first remove titles for the file in the database.
Returns the number of rows inserted."
  (let* ((file (or org-roam-file-name (buffer-file-name)))
         (titles (or (org-roam--extract-titles)
                     (list (org-roam--path-to-slug file))))
         (rows (mapcar (lambda (title)
                         (vector file title)) titles)))
    (when update-p
      (org-roam-db-query [:delete :from titles
                          :where (= file $s1)]
                         file))
    (org-roam-db-query
        [:insert :into titles
         :values $v1]
        rows)
    (length rows)))

(defun org-roam-db--insert-refs (&optional update-p)
  "Update the refs of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the ref for the file in the database."
  (let ((file (or org-roam-file-name (buffer-file-name)))
        (count 0))
    (when update-p
      (org-roam-db-query [:delete :from refs
                          :where (= file $s1)]
                         file))
    (when-let ((refs (org-roam--extract-refs)))
      (dolist (ref refs)
        (let ((key (cdr ref))
              (type (car ref)))
          (condition-case nil
              (progn
                (org-roam-db-query
                 [:insert :into refs :values $v1]
                 (list (vector key file type)))
                (cl-incf count))
            (error
             (lwarn '(org-roam) :error
                    (format "Duplicate ref %s in:\n\nA: %s\nB: %s\n\nskipping..."
                            key
                            file
                            (caar (org-roam-db-query
                                   [:select file :from refs
                                    :where (= ref $v1)]
                                   (vector key))))))))))
    count))

(defun org-roam-db--insert-links (&optional update-p)
  "Update the file links of the current buffer in the cache.
If UPDATE-P is non-nil, first remove the links for the file in the database.
Return the number of rows inserted."
  (let ((file (or org-roam-file-name (buffer-file-name))))
    (when update-p
      (org-roam-db-query [:delete :from links
                          :where (= source $s1)]
                         file))
    (if-let ((links (org-roam--extract-links)))
        (progn
          (org-roam-db-query
           [:insert :into links
            :values $v1]
           links)
          (length links))
      0)))

(defun org-roam-db--insert-ids (&optional update-p)
  "Update the ids of the current buffer into the cache.
If UPDATE-P is non-nil, first remove ids for the file in the database.
Returns the number of rows inserted."
  (let ((file (or org-roam-file-name (buffer-file-name))))
    (when update-p
      (org-roam-db-query [:delete :from ids
                          :where (= file $s1)]
                         file))
    (if-let ((ids (org-roam--extract-ids file)))
      (condition-case nil
          (progn
            (org-roam-db-query
             [:insert :into ids
              :values $v1]
             ids)
            (length ids))
        (error
         (lwarn '(org-roam) :error
                (format "Duplicate IDs in %s, one of:\n\n%s\n\nskipping..."
                        (aref (car ids) 1)
                        (string-join (mapcar (lambda (hl)
                                               (aref hl 0)) ids) "\n")))
         0))
      0)))

(defun org-roam-db--insert-tags (&optional update-p)
  "Insert tags for the current buffer into the Org-roam cache.
If UPDATE-P is non-nil, first remove tags for the file in the database.
Return the number of rows inserted."
  (let* ((file (or org-roam-file-name (buffer-file-name)))
         (tags (org-roam--extract-tags file)))
    (when update-p
      (org-roam-db-query [:delete :from tags
                          :where (= file $s1)]
                         file))
    (if tags
        (progn (org-roam-db-query
                [:insert :into tags
                 :values $v1]
                (list (vector file tags)))
               1)
      0)))

;;;;; Fetching
(defun org-roam-db-has-file-p (file)
  "Return t if FILE is in the database, nil otherwise."
  (> (caar (org-roam-db-query [:select (funcall count) :from files
                              :where (= file $s1)]
                              file))
     0))

(defun org-roam-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let* ((current-files (org-roam-db-query [:select * :from files]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-db--get-title (file)
  "Return the main title of FILE from the cache."
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
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"dest\", \"file\" FROM citelinks),
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
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"source\", \"file\" FROM citelinks),
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
  (if file-path
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file-path)
        (secure-hash 'sha1 (current-buffer)))
    (org-with-wide-buffer
     (secure-hash 'sha1 (current-buffer)))))

;;;;; Updating
(defun org-roam-db--update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path
                      (buffer-file-name (buffer-base-buffer))))
  (if (not (file-exists-p file-path))
      (org-roam-db--clear-file file-path)
    ;; save the file before performing a database update
    (when-let ((buf (find-buffer-visiting file-path)))
      (with-current-buffer buf
        (save-buffer)))
    (org-roam--with-temp-buffer file-path
      (emacsql-with-transaction (org-roam-db)
        (org-roam-db--insert-meta 'update)
        (org-roam-db--insert-tags 'update)
        (org-roam-db--insert-titles 'update)
        (org-roam-db--insert-refs 'update)
        (when org-roam-enable-headline-linking
          (org-roam-db--insert-ids 'update))
        (org-roam-db--insert-links 'update)))))

(defun org-roam-db-build-cache (&optional force)
  "Build the cache for `org-roam-directory'.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file org-roam-db-location))
  (org-roam-db--close) ;; Force a reconnect
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam--list-all-files))
         (current-files (org-roam-db--get-current-files))
         (count-plist nil)
         (deleted-count 0)
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                       contents-hash)
          (push (cons file contents-hash) modified-files)))
      (remhash file current-files))
    (dolist (file (hash-table-keys current-files))
        ;; These files are no longer around, remove from cache...
        (org-roam-db--clear-file file)
        (setq deleted-count (1+ deleted-count)))
    (setq count-plist (org-roam-db--update-files modified-files))
    (org-roam-message "total: Δ%s, files-modified: Δ%s, ids: Δ%s, links: Δ%s, tags: Δ%s, titles: Δ%s, refs: Δ%s, deleted: Δ%s"
                      (- (length org-roam-files) (plist-get count-plist :error-count))
                      (plist-get count-plist :modified-count)
                      (plist-get count-plist :id-count)
                      (plist-get count-plist :link-count)
                      (plist-get count-plist :tag-count)
                      (plist-get count-plist :title-count)
                      (plist-get count-plist :ref-count)
                      deleted-count)))

(defun org-roam-db--get-file-hash-from-db (&optional file-path)
  "Get hash from Org-roam database for FILE-PATH."
  (setq file-path (or file-path
                      (buffer-file-name (buffer-base-buffer))))
  (caar (org-roam-db-query [:select hash :from files
                              :where (= file $s1)] file-path)))

(defun org-roam-db-update-file (file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash  (org-roam-db--get-file-hash-from-db file-path)))
    (unless (string= content-hash db-hash)
      (org-roam-db--update-files (list (cons file-path content-hash)))
      (org-roam-message "Updated: %s" file-path))))

(defun org-roam-db--update-files (modified-files)
  "Update Org-roam cache for a list of MODIFIED-FILES.
FILES is a list of (file . hash) pairs."
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (error-count 0)
         (id-count 0)
         (link-count 0)
         (tag-count 0)
         (title-count 0)
         (ref-count 0)
         (modified-count 0))
    (pcase-dolist (`(,file . _) modified-files)
      (org-roam-db--clear-file file))
    ;; Process all the files for IDs first
    ;;
    ;; We do this so that link extraction is cheaper: this eliminates the need
    ;; to read the file to check if the ID really exists
    (pcase-dolist (`(,file . ,contents-hash) modified-files)
      (let* ((attr (file-attributes file))
             (atime (file-attribute-access-time attr))
             (mtime (file-attribute-modification-time attr)))
        (condition-case nil
            (org-roam--with-temp-buffer file
              (org-roam-db-query
               [:insert :into files
                :values $v1]
               (vector file contents-hash (list :atime atime :mtime mtime)))
              (when org-roam-enable-headline-linking
                (setq id-count (+ id-count (org-roam-db--insert-ids)))))
          (file-error
           (setq error-count (1+ error-count))
           (org-roam-db--clear-file file)
           (lwarn '(org-roam) :warning
                  "Skipping unreadable file while building cache: %s" file)))))

    ;; Process titles, tags, links and ref links of file
    (pcase-dolist (`(,file . _) modified-files)
      (org-roam-message "Processed %s/%s modified files..." modified-count (length modified-files))
      (condition-case nil
          (org-roam--with-temp-buffer file
            (setq modified-count (1+ modified-count))
            (setq link-count (+ link-count (org-roam-db--insert-links)))
            (setq tag-count (+ tag-count (org-roam-db--insert-tags)))
            (setq title-count (+ title-count (org-roam-db--insert-titles)))
            (setq ref-count (+ ref-count (org-roam-db--insert-refs))))
        (file-error
         (setq error-count (1+ error-count))
         (org-roam-db--clear-file file)
         (lwarn '(org-roam) :warning
                "Skipping unreadable file while building cache: %s" file))))
    (list :error-count error-count :modified-count modified-count :id-count id-count :title-count title-count :tag-count tag-count :link-count link-count :ref-count ref-count)))

(defun org-roam-db-update ()
  "Update the database."
  (pcase org-roam-db-update-method
    ('immediate
     (org-roam-db-update-file (buffer-file-name (buffer-base-buffer))))
    ('idle-timer
     (org-roam-db-mark-dirty))
    (_
     (user-error "Invalid `org-roam-db-update-method'"))))

(provide 'org-roam-db)

;;; org-roam-db.el ends here
