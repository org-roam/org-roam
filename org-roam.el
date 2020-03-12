;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Library Requires
(require 'org)
(require 'org-element)
(require 'org-capture)
(require 'ob-core)  ;for org-babel-parse-header-arguments
(require 'subr-x)
(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)

;; Database requirements
(require 'emacsql)
(require 'emacsql-sqlite)

(require 'xml) ; for xml-parse-string

;;;; Customizable Variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/jethrokuan/org-roam")
  :link '(url-link :tag "Online Manual" "https://org-roam.readthedocs.io/"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.

All Org files, at any level of nesting, is considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-buffer-position 'right
  "Position of `org-roam' buffer.
Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The formatter used when inserting Org-roam links that use their title.
Formatter may be a function that takes title as its only argument."
  :type '(choice
          (string :tag "String Format" "%s")
          (function :tag "Custom function"))
  :group 'org-roam)

(defcustom org-roam-buffer-width 0.33 "Width of `org-roam' buffer."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer "*org-roam*"
  "Org-roam buffer name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .org.gpg extension."
  :type 'boolean
  :group 'org-roam)

;;;; Dynamic variables
(defvar org-roam--current-buffer nil
  "Currently displayed file in `org-roam' buffer.")

(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

;;; Database
;;;; Options
(defconst org-roam-db-filename "org-roam.db"
  "Name of the Org-roam database file.")

(defconst org-roam--db-version 1)
(defconst org-roam--sqlite-available-p
  (with-demoted-errors "Org-roam initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam--db-connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;;; Core Functions
(defun org-roam--get-db ()
  "Return the sqlite db file."
  (interactive "P")
  (expand-file-name org-roam-db-filename org-roam-directory))

(defun org-roam--get-db-connection ()
  "Return the database connection, if any."
  (gethash (file-truename org-roam-directory)
           org-roam--db-connection))

(defun org-roam-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam--get-db-connection)
               (emacsql-live-p (org-roam--get-db-connection)))
    (let* ((db-file (org-roam--get-db))
           (init-db (not (file-exists-p db-file))))
      (make-directory (file-name-directory db-file) t)
      (let ((conn (emacsql-sqlite db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
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
             "The Org-roam database was created with a newer Org-roam version.  "
             "You need to update the Org-roam package"))
           ((< version org-roam--db-version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  (org-roam--get-db-connection))

;;;; Entrypoint: (org-roam-sql)
(defun org-roam-sql (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if  (stringp sql)
      (emacsql (org-roam-db) (apply #'format sql args))
    (apply #'emacsql (org-roam-db) sql args)))

;;;; Schemata
(defconst org-roam--db-table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      (last-modified :not-null)
      ])

    (file-links
     [(file-from :not-null)
      (file-to :not-null)
      (properties :not-null)])

    (titles
     [
      (file :not-null)
      titles])

    (refs
     [(ref :unique :not-null)
      (file :not-null)])))

(defun org-roam--db-init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) org-roam--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" org-roam--db-version))))

(defun org-roam--db-maybe-update (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    ;; Do nothing now
    version))

(defun org-roam--db-close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-roam-directory'."
  (unless db
    (setq db (org-roam--get-db-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun org-roam--db-close-all ()
  "Closes all database connections made by Org-roam."
  (dolist (conn (hash-table-values org-roam--db-connection))
    (org-roam--db-close conn)))

;;;; Database API
;;;;; Initialization
(defun org-roam--db-initialized-p ()
  "Whether the cache has been initialized."
  (and (file-exists-p (org-roam--get-db))
       (> (caar (org-roam-sql [:select (funcall count) :from titles]))
          0)))

(defun org-roam--db-ensure-built ()
  "Ensures that Org-roam cache is built."
  (unless (org-roam--db-initialized-p)
    (error "[Org-roam] your cache isn't built yet! Please run org-roam-build-cache")))

;;;;; Clearing
(defun org-roam--db-clear ()
  "Clears all entries in the caches."
  (interactive)
  (when (file-exists-p (org-roam--get-db))
    (org-roam-sql [:delete :from files])
    (org-roam-sql [:delete :from titles])
    (org-roam-sql [:delete :from file-links])
    (org-roam-sql [:delete :from refs])))

(defun org-roam--db-clear-file (&optional filepath)
  "Remove any related links to the file at FILEPATH.
This is equivalent to removing the node from the graph."
  (let* ((path (or filepath
                   (buffer-file-name)))
         (file (file-truename path)))
    (org-roam-sql [:delete :from files
                   :where (= file $s1)]
                  file)
    (org-roam-sql [:delete :from file-links
                   :where (= file-from $s1)]
                  file)
    (org-roam-sql [:delete :from titles
                   :where (= file $s1)]
                  file)
    (org-roam-sql [:delete :from refs
                   :where (= file $s1)]
                  file)))

;;;;; Insertion
(defun org-roam--db-insert-links (links)
  "Insert LINKS into the Org-roam cache."
  (org-roam-sql
   [:insert :into file-links
    :values $v1]
   links))

(defun org-roam--db-insert-titles (file titles)
  "Insert TITLES for a FILE into the Org-roam cache."
  (org-roam-sql
   [:insert :into titles
    :values $v1]
   (list (vector file titles))))

(defun org-roam--db-insert-ref (file ref)
  "Insert REF for FILE into the Org-roam cache."
  (org-roam-sql
   [:insert :into refs
    :values $v1]
   (list (vector ref file))))

;;;;; Fetching
(defun org-roam--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let* ((current-files (org-roam-sql [:select * :from files]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam--db-get-titles (file)
  "Return the titles of FILE from the cache."
  (caar (org-roam-sql [:select [titles] :from titles
                       :where (= file $s1)]
                      file
                      :limit 1)))

;;;;; Updating
(defun org-roam--db-update-titles ()
  "Update the title of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-sql [:delete :from titles
                   :where (= file $s1)]
                  file)
    (org-roam--db-insert-titles file (org-roam--extract-titles))))

(defun org-roam--db-update-refs ()
  "Update the ref of the current buffer into the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-sql [:delete :from refs
                   :where (= file $s1)]
                  file)
    (when-let ((ref (org-roam--extract-ref)))
      (org-roam--db-insert-ref file ref))))

(defun org-roam--update-cache-links ()
  "Update the file links of the current buffer in the cache."
  (let ((file (file-truename (buffer-file-name))))
    (org-roam-sql [:delete :from file-links
                   :where (= file-from $s1)]
                  file)
    (when-let ((links (org-roam--extract-links)))
      (org-roam--db-insert-links links))))

(defun org-roam--db-update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH."
  (let (buf)
    (if file-path
        (setq buf (find-file-noselect file-path))
      (setq buf (current-buffer)))
    (with-current-buffer buf
      (save-excursion
        (org-roam--db-update-titles)
        (org-roam--db-update-refs)
        (org-roam--update-cache-links)
        (org-roam--maybe-update-buffer :redisplay t)))))

;;;;; org-roam-build-cache
(defun org-roam-build-cache ()
  "Build the cache for `org-roam-directory'."
  (interactive)
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((org-roam-files (org-roam--list-files org-roam-directory))
         (current-files (org-roam--get-current-files))
         (time (current-time))
         all-files all-links all-titles all-refs)
    (dolist (file org-roam-files)
      (org-roam--with-temp-buffer
        (insert-file-contents file)
        (let ((contents-hash (secure-hash 'sha1 (current-buffer))))
          (unless (string= (gethash file current-files)
                           contents-hash)
            (org-roam--db-clear-file file)
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
      (org-roam--db-clear-file file))
    (when all-files
      (org-roam-sql
       [:insert :into files
        :values $v1]
       all-files))
    (when all-links
      (org-roam-sql
       [:insert :into file-links
        :values $v1]
       all-links))
    (when all-titles
      (org-roam-sql
       [:insert :into titles
        :values $v1]
       all-titles))
    (when all-refs
      (org-roam-sql
       [:insert :into refs
        :values $v1]
       all-refs))
    (let ((stats (list :files (length all-files)
                       :links (length all-links)
                       :titles (length all-titles)
                       :refs (length all-refs)
                       :deleted (length (hash-table-keys current-files)))))
      (message "files: %s, links: %s, titles: %s, refs: %s, deleted: %s"
               (plist-get stats :files)
               (plist-get stats :links)
               (plist-get stats :titles)
               (plist-get stats :refs)
               (plist-get stats :deleted))
      stats)))

;;; Utilities
;;;; General Utilities
(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam--aliases-str-to-list (str)
  "Function to transform string STR into list of alias titles.

This snippet is obtained from ox-hugo:
https://github.com/kaushalmodi/ox-hugo/blob/a80b250987bc770600c424a10b3bca6ff7282e3c/ox-hugo.el#L3131"
  (when (stringp str)
    (let* ((str (org-trim str))
           (str-list (split-string str "\n"))
           ret)
      (dolist (str-elem str-list)
        (let* ((format-str ":dummy '(%s)") ;The :dummy key is discarded in the `lst' var below.
               (alist (org-babel-parse-header-arguments (format format-str str-elem)))
               (lst (cdr (car alist)))
               (str-list2 (mapcar (lambda (elem)
                                    (cond
                                     ((symbolp elem)
                                      (symbol-name elem))
                                     (t
                                      elem)))
                                  lst)))
          (setq ret (append ret str-list2))))
      ret)))

(defmacro org-roam--with-temp-buffer (&rest body)
  "Call `with-temp-buffer', propagating `org-roam-directory' to
the temp buffer."
  (declare (indent 0) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory))
           ,@body)))))

;;;; File functions and predicates
(defun org-roam--touch-file (path)
  "Touches an empty file at PATH."
  (make-directory (file-name-directory path) t)
  (f-touch path))

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (org-roam--file-name-extension path)))
    (or (string= ext "org")
        (and
         (string= ext "gpg")
         (string= (org-roam--file-name-extension (file-name-sans-extension path)) "org")))))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let ((path (or file
                  (buffer-file-name))))
    (and path
         (org-roam--org-file-p path)
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located within DIR, at any nesting level.
Ignores hidden files and directories."
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            (dir-ignore-regexp (concat "\\(?:"
                                       "\\."
                                       "\\|\\.\\."
                                       "\\)$"))
            result)
        (dolist (file files)
          (cond
           ((file-directory-p file)
            (unless (string-match dir-ignore-regexp file)
              (setq result (append (org-roam--list-files file) result))))
           ((and (file-readable-p file)
                 (org-roam--org-file-p file))
            (setq result (cons (file-truename file) result)))))
        result)))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (file-truename org-roam-directory)))

;;;; Org extraction functions
(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current org buffer.
The search terminates when the first property is encountered."
  (let ((buf (org-element-parse-buffer))
        res)
    (dolist (prop props)
      (let ((p (org-element-map buf 'keyword
                 (lambda (kw)
                   (when (string= (org-element-property :key kw) prop)
                     (org-element-property :value kw)))
                 :first-match t)))
        (push (cons prop p) res)))
    res))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [file-from file-to properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name)))))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link))
              (start (org-element-property :begin link)))
          (when (and (string= type "file")
                     (org-roam--org-file-p path))
            (goto-char start)
            (let* ((element (org-element-at-point))
                   (begin (or (org-element-property :content-begin element)
                              (org-element-property :begin element)))
                   (content (or (org-element-property :raw-value element)
                                (buffer-substring
                                 begin
                                 (or (org-element-property :content-end element)
                                     (org-element-property :end element)))))
                   (content (string-trim content)))
              (vector file-path
                      (file-truename (expand-file-name path (file-name-directory file-path)))
                      (list :content content :point begin)))))))))

(defun org-roam--extract-titles ()
  "Extract the titles from current buffer.
Titles are obtained via the #+TITLE property, or aliases
specified via the #+ROAM_ALIAS property."
  (let* ((props (org-roam--extract-global-props '("TITLE" "ROAM_ALIAS")))
         (aliases (cdr (assoc "ROAM_ALIAS" props)))
         (title (cdr (assoc "TITLE" props)))
         (alias-list (org-roam--aliases-str-to-list aliases)))
    (if title
        (cons title alias-list)
      alias-list)))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer."
  (cdr (assoc "ROAM_KEY" (org-roam--extract-global-props '("ROAM_KEY")))))
;;;; Title/Path/Slug conversion
(defun org-roam--path-to-slug (path)
  "Return a slug from PATH."
  (-> path
      (file-relative-name (file-truename org-roam-directory))
      (file-name-sans-extension)))

(defun org-roam--get-title-or-slug (path)
  "Convert `PATH' to the file title, if it exists.  Else, return the path."
  (or (car (org-roam--db-get-titles path))
      (org-roam--path-to-slug path)))

(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (replace (title pair)
                      (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'replace (strip-nonspacing-marks title) pairs)))
      (s-downcase slug))))

;;;; Org-roam capture
(defun org-roam--file-path-from-id (id)
  "The file path for an Org-roam file, with identifier ID."
  (file-truename
   (expand-file-name
    (if org-roam-encrypt-files
        (concat id ".org.gpg")
      (concat id ".org"))
    org-roam-directory)))

(defvar org-roam--capture-file-name-default "%<%Y%m%d%H%M%S>"
  "The default file name format for Org-roam templates.")

(defvar org-roam--capture-header-default "#+TITLE: ${title}\n"
  "The default capture header for Org-roam templates.")

(defvar org-roam--capture-file-path nil
  "The file path for the Org-roam capture.
This variable is set during the Org-roam capture process.")

(defvar org-roam--capture-info nil
  "An alist of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam--capture-context nil
  "A symbol, that reflects the context for obtaining the exact point in a file.
This variable is populated dynamically, and is only active during
an Org-roam capture process.

The `title' context is used in `org-roam-insert' and
`org-roam-find-file', where the capture process is triggered upon
trying to create a new file without that `title'.

The `ref' context is used by `org-roam-protocol', where the
capture process is triggered upon trying to find or create a new
note with the given `ref'.")

(defvar org-roam-capture-templates
  '(("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+TITLE: ${title}\n"
     :unnarrowed t))
  "Capture templates for Org-roam.
The capture templates are an extension of
`org-capture-templates', and the documentation there also
applies.

`org-capture-templates' are extended in 3 ways:

1. Template expansion capabilities are extended with additional custom syntax.
   See `org-roam--fill-template' for more details.

2. The `:file-name' key is added, which expands to the file-name
   of the note if it creates a new file.  This file-name is
   relative to `org-roam-directory', and is without the
   file-extension.

3. The `:head' key is added, which contains the template that is
   inserted on initial creation (added only once).  This is where
   insertion of any note metadata should go.")

(defun org-roam--fill-template (str &optional info)
  "Expands the template STR, returning the string.
This is an extension of org-capture's template expansion.

First, it expands ${var} occurences in STR, using the INFO alist.
If there is a ${var} with no matching var in the alist, the value
of var is prompted for via `completing-read'.

Next, it expands the remaining template string using
`org-capture-fill-template'."
  (-> str
      (s-format (lambda (key)
                  (or (s--aget info key)
                      (completing-read (format "%s: " key ) nil))) nil)
      (org-capture-fill-template)))

(defun org-roam--capture-new-file ()
  "Create a new file and return the file path.
This is achieved by reading the file-name attribute of the
currently active Org-roam template."
  (let* ((name-templ (or (org-capture-get :file-name)
                         org-roam--capture-file-name-default))
         (new-id (s-trim (org-roam--fill-template
                          name-templ
                          org-roam--capture-info)))
         (file-path (org-roam--file-path-from-id new-id)))
    (when (file-exists-p file-path)
      (error (format "File exists at %s, aborting" file-path)))
    (org-roam--touch-file file-path)
    (write-region
     (org-roam--fill-template (or (org-capture-get :head)
                                  org-roam--capture-header-default)
                              org-roam--capture-info)
     nil file-path nil)
    (sleep-for 0.2)  ;; Hack: expand-file-name stringp nil error sporadically otherwise
    file-path))

(defun org-roam--capture-get-point ()
  "Return exact point to file for org-capture-template.
The file to use is dependent on the context:

If the search is via title, it is assumed that the file does not
yet exist, and Org-roam will attempt to create new file.

If the search is via ref, it is matched against the Org-roam database.
If there is no file with that ref, a file with that ref is created.

This function is used solely in Org-roam's capture templates: see
`org-roam-capture-templates'."
  (pcase org-roam--capture-context
    ('title
     (let ((file-path (org-roam--capture-new-file)))
       (setq org-roam--capture-file-path file-path)
       (set-buffer (org-capture-target-buffer file-path))
       (widen)
       (goto-char (point-max))))
    ('ref
     (let* ((completions (org-roam--get-ref-path-completions))
            (ref (cdr (assoc 'ref org-roam--capture-info)))
            (file-path (or (cdr (assoc ref completions))
                           (org-roam--capture-new-file))))
       (setq org-roam--capture-file-path file-path)
       (set-buffer (org-capture-target-buffer file-path))
       (widen)
       (goto-char (point-max))))
    (_ (error "Invalid org-roam-capture-context"))))

(defun org-roam-capture (&optional goto keys)
  "Create a new file, and return the path to the edited file.
The templates are defined at `org-roam-capture-templates'.  The
GOTO and KEYS argument have the same functionality as
`org-capture'."
  (let ((org-capture-templates org-roam-capture-templates)
        file-path)
    (when (= (length org-capture-templates) 1)
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)
    (setq file-path org-roam--capture-file-path)
    (setq org-roam--capture-file-path nil)
    file-path))

;;; Interactive Commands
;;;; org-roam-insert
(defvar org-roam--capture-insert-point nil
  "The point to jump to after the call to `org-roam-insert'.")

(defun org-roam--format-link-title (title)
  "Retur the link title, given the file TITLE."
  (if (functionp org-roam-link-title-format)
      (funcall org-roam-link-title-format title)
    (format org-roam-link-title-format title)))

(defun org-roam--insert-with-completion-method (prefix chooser)
  (unless (org-roam--org-roam-file-p
           (buffer-file-name (buffer-base-buffer)))
    (user-error "Not in an Org-roam file"))
  (let* ((region (and (region-active-p)
                      ;; following may lose active region, so save it
                      (cons (region-beginning) (region-end))))
         (region-text (when region
                        (buffer-substring-no-properties
                         (car region) (cdr region))))
         (completions (org-roam--get-title-path-completions))
         (title (funcall chooser completions region-text))
         (region-or-title (or region-text title))
         (target-file-path (cdr (assoc title completions)))
         (current-file-path (-> (or (buffer-base-buffer)
                                    (current-buffer))
                                (buffer-file-name)
                                (file-truename)
                                (file-name-directory)))
         (buf (current-buffer))
         (p (point-marker)))
    (unless (and target-file-path
                 (file-exists-p target-file-path))
      (let* ((org-roam--capture-info (list (cons 'title title)
                                           (cons 'slug (org-roam--title-to-slug title))))
             (org-roam--capture-context 'title))
        (setq target-file-path (org-roam-capture))))
    (with-current-buffer buf
      (when region ;; Remove previously selected text.
        (delete-region (car region) (cdr region)))
      (let ((link-location (concat "file:" (file-relative-name target-file-path current-file-path)))
            (description (org-roam--format-link-title (if prefix
                                                          (downcase region-or-title)
                                                        region-or-title))))
        (goto-char p)
        (insert (format "[[%s][%s]]"
                        link-location
                        description))
        (setq org-roam--capture-insert-point (point))))))

(defun org-roam-insert (prefix)
  "Find an Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method
   prefix
   #'(lambda (completions region-text)
       (completing-read "File: " completions nil nil region-text))))

(defun org-roam--capture-advance-point ()
  "Advances the point if it is updated.

We need this function because typically `org-capture' prevents the
point from being advanced, whereas when a link is inserted, the
point moves some characters forward.  This is added as a hook to
`org-capture-after-finalize-hook'."
  (when org-roam--capture-insert-point
    (goto-char org-roam--capture-insert-point)
    (setq org-roam--capture-insert-point nil)))

(add-hook 'org-capture-after-finalize-hook #'org-roam--capture-advance-point)

;;;; org-roam-find-file
(defun org-roam--get-title-path-completions ()
  "Return a list of cons pairs for titles to absolute path of Org-roam files."
  (let* ((rows (org-roam-sql [:select [file titles] :from titles]))
         res)
    (dolist (row rows)
      (let ((file-path (car row))
            (titles (cadr row)))
        (if titles
            (dolist (title titles)
              (push (cons title file-path) res))
          (push (cons (org-roam--path-to-slug file-path)
                      file-path) res))))
    res))

(defun org-roam--find-file-with-completion-method (chooser)
  "Find and open an org-roam file using completion method CHOOSER."
  (let* ((completions (org-roam--get-title-path-completions))
         (title (funcall chooser completions))
         (file-path (cdr (assoc title completions))))
    (if file-path
        (find-file file-path)
      (let* ((org-roam--capture-info (list (cons 'title title)
                                           (cons 'slug (org-roam--title-to-slug title))))
             (org-roam--capture-context 'title))
        (org-roam-capture '(4))))))

(defun org-roam-find-file (&optional initial-prompt)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt."
  (interactive)
  (org-roam--find-file-with-completion-method
   #'(lambda (completions)
       (completing-read "File: " completions nil nil initial-prompt))))

;;;; org-roam-find-ref
(defun org-roam--get-ref-path-completions ()
  "Return a list of cons pairs for titles to absolute path of Org-roam files."
  (let ((rows (org-roam-sql [:select [ref file] :from refs])))
    (mapcar (lambda (row)
              (cons (car row)
                    (cadr row))) rows)))

(defun org-roam-find-ref (&optional info)
  "Find and open an Org-roam file from a ref.
INFO is an alist containing additional information."
  (interactive)
  (let* ((completions (org-roam--get-ref-path-completions))
         (ref (or (cdr (assoc 'ref info))
                  (completing-read "Ref: " (org-roam--get-ref-path-completions) nil t))))
    (find-file (cdr (assoc ref completions)))))

;;;; org-roam-switch-to-buffer
(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam-switch-to-buffer ()
  "Switch to an existing Org-roam buffer."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (org-roam--get-title-or-slug
                                                 (buffer-file-name buffer))
                                                (buffer-name buffer))
                                            buffer))
                                    roam-buffers)))
    (unless roam-buffers
      (user-error "No roam buffers"))
    (when-let ((name (completing-read "Choose a buffer: " names-and-buffers)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

;;;; Daily notes
(defun org-roam--file-for-time (time)
  "Create and find file for TIME."
  (let* ((title (format-time-string "%Y-%m-%d" time))
         (file-path (org-roam--file-path-from-id title)))
    (if (file-exists-p file-path)
        file-path
      (let ((org-roam-capture-templates (list (list "d" "daily" 'plain (list 'function #'org-roam--capture-get-point)
                                                    ""
                                                    :immediate-finish t
                                                    :file-name "${title}"
                                                    :head "#+TITLE: ${title}")))
            (org-roam--capture-context 'title)
            (org-roam--capture-info (list (cons 'title title))))
        (org-roam-capture)))))

(defun org-roam-today ()
  "Create and find file for today."
  (interactive)
  (let ((path (org-roam--file-for-time (current-time))))
    (org-roam--find-file path)))

(defun org-roam-tomorrow ()
  "Create and find the file for tomorrow."
  (interactive)
  (let ((path (org-roam--file-for-time (time-add 86400 (current-time)))))
    (org-roam--find-file path)))

(defun org-roam-yesterday ()
  "Create and find the file for yesterday."
  (interactive)
  (let ((path (org-roam--file-for-time (time-add -86400 (current-time)))))
    (org-roam--find-file path)))

(defun org-roam-date ()
  "Create the file for any date using the calendar."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (let ((path (org-roam--file-for-time time)))
      (org-roam--find-file path))))

;;; The org-roam buffer
;;;; org-roam-link-face
(defface org-roam-link
  '((t :inherit org-link))
  "Face for org-roam link."
  :group 'org-roam-faces)

(defface org-roam-backlink
  '((t :inherit org-block))
  "Face for org-roam backlinks in backlinks buffer"
  :group 'org-roam-faces)

(defun org-roam--roam-link-face (path)
  "Conditional face for org file links.
Applies `org-roam-link-face' if PATH correponds to a Roam file."
  (if (org-roam--org-roam-file-p path)
      'org-roam-link
    'org-link))

;;;; org-roam-backlinks-mode
(defvar org-roam-backlinks-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-1] 'org-roam-open-at-point)
    (define-key km (kbd "RET") 'org-roam-open-at-point)
    km)
  "Keymap for `org-roam-backlinks-mode'.")

(define-derived-mode org-roam-backlinks-mode org-mode "Backlinks"
  "Major mode for the `org-roam-buffer'.

\\{org-roam-backlinks-mode-map}")

(defun org-roam-open-at-point ()
  "Open a link at point.

When point is on an Org-roam link, open the link in the Org-roam window.

When point is on the Org-roam preview text, open the link in the Org-roam
window, and navigate to the point.

If item at point is not Org-roam specific, default to Org behaviour."
  (interactive)
  (let ((context (org-element-context)))
    (catch 'ret
      ;; Org-roam link
      (when (and (eq (org-element-type context) 'link)
                 (string= "file" (org-element-property :type context))
                 (org-roam--org-roam-file-p (file-truename (org-element-property :path context))))
        (org-roam--find-file (org-element-property :path context))
        (org-show-context)
        (throw 'ret t))
      ;; Org-roam preview text
      (when-let ((file-from (get-text-property (point) 'file-from))
                 (p (get-text-property (point) 'file-from-point)))
        (org-roam--find-file file-from)
        (goto-char p)
        (org-show-context)
        (throw 'ret t))
      ;; Default to default org behaviour
      (org-open-at-point))))

(defun org-roam--find-file (file)
  "Open FILE in the window `org-roam' was called from."
  (if (and org-roam-last-window (window-valid-p org-roam-last-window))
      (progn (with-selected-window org-roam-last-window
               (find-file file))
             (select-window org-roam-last-window))
    (find-file file)))

(defun org-roam--get-backlinks (file)
  "Return the backlinks for FILE."
  (org-roam-sql [:select [file-from, file-to, properties] :from file-links
                 :where (= file-to $s1)]
                file))

;;;; Updating the org-roam buffer
(defun org-roam-update (file-path)
  "Show the backlinks for given org file for file at `FILE-PATH'."
  (org-roam--db-ensure-built)
  (let* ((source-org-roam-directory org-roam-directory))
    (let ((buffer-title (org-roam--get-title-or-slug file-path)))
      (with-current-buffer org-roam-buffer
        ;; When dir-locals.el is used to override org-roam-directory,
        ;; org-roam-buffer should have a different local org-roam-directory and
        ;; default-directory, as relative links are relative from the overridden
        ;; org-roam-directory.
        (setq-local org-roam-directory source-org-roam-directory)
        (setq-local default-directory source-org-roam-directory)
        ;; Locally overwrite the file opening function to re-use the
        ;; last window org-roam was called from
        (setq-local
         org-link-frame-setup
         (cons '(file . org-roam--find-file) org-link-frame-setup))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (eq major-mode 'org-roam-backlinks-mode)
            (org-roam-backlinks-mode))
          (make-local-variable 'org-return-follows-link)
          (setq org-return-follows-link t)
          (insert
           (propertize buffer-title 'font-lock-face 'org-document-title))
          (if-let* ((backlinks (org-roam--get-backlinks file-path))
                    (grouped-backlinks (--group-by (nth 0 it) backlinks)))
              (progn
                (insert (format "\n\n* %d Backlinks\n"
                                (length backlinks)))
                (dolist (group grouped-backlinks)
                  (let ((file-from (car group))
                        (bls (cdr group)))
                    (insert (format "** [[file:%s][%s]]\n"
                                    file-from
                                    (org-roam--get-title-or-slug file-from)))
                    (dolist (backlink bls)
                      (pcase-let ((`(,file-from _ ,props) backlink))
                        (insert (propertize
                                 (s-trim (s-replace "\n" " "
                                                    (plist-get props :content)))
                                 'font-lock-face 'org-roam-backlink
                                 'help-echo "mouse-1: visit backlinked note"
                                 'file-from file-from
                                 'file-from-point (plist-get props :point)))
                        (insert "\n\n"))))))
            (insert "\n\n* No backlinks!")))
        (read-only-mode 1)))))

(cl-defun org-roam--maybe-update-buffer (&key redisplay)
  "Reconstructs `org-roam-buffer'.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
`org-roam-buffer'."
  (let ((buffer (window-buffer)))
    (when (and (or redisplay
                   (not (eq org-roam--current-buffer buffer)))
               (eq 'visible (org-roam--current-visibility))
               (buffer-local-value 'buffer-file-truename buffer))
      (setq org-roam--current-buffer buffer)
      (org-roam-update (expand-file-name
                        (buffer-local-value 'buffer-file-truename buffer))))))

;;;; Toggling the org-roam buffer
(define-inline org-roam--current-visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam--set-width (width)
  "Set the width of `org-roam-buffer' to `WIDTH'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun org-roam--setup-buffer ()
  "Setup the `org-roam' buffer at the `org-roam-buffer-position'."
  (let ((window (get-buffer-window)))
    (-> (get-buffer-create org-roam-buffer)
        (display-buffer-in-side-window
         `((side . ,org-roam-buffer-position)))
        (select-window))
    (org-roam--set-width
     (round (* (frame-width)
               org-roam-buffer-width)))
    (select-window window)))

(defun org-roam ()
  "Pops up the window `org-roam-buffer' accordingly."
  (interactive)
  (setq org-roam-last-window (get-buffer-window))
  (pcase (org-roam--current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam--setup-buffer))
    ('none (org-roam--setup-buffer))))

;;; The graphviz links graph
;;;; Options
(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Path to executable for viewing SVG."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graphviz-executable (executable-find "dot")
  "Path to graphviz executable."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graphviz-extra-options nil
  "Extra options when contructing the Graphviz graph.
Example:
 '((\"rankdir\" . \"LR\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-max-title-length 100
  "Maximum length of titles in Graphviz graph nodes."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-graph-exclude-matcher nil
  "String for excluding nodes from the generated graph.
Any nodes and links for file paths matching this string is
excluded from the graph."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-node-shape "ellipse"
  "Shape of Graphviz nodes."
  :type 'string
  :group 'org-roam)

;;;; Functions
(defun org-roam--build-graph ()
  "Build the Graphviz string.
The Org-roam database titles table is read, to obtain the list of titles.
The file-links table is then read to obtain all directed links, and formatted
into a digraph."
  (org-roam--db-ensure-built)
  (org-roam--with-temp-buffer
    (let* ((matcher (concat "%" org-roam-graph-exclude-matcher "%"))
           (nodes (if org-roam-graph-exclude-matcher
                      (org-roam-sql [:select [file titles]
                                     :from titles
                                     :where file :not :like $s1]
                                    matcher)
                    (org-roam-sql [:select [file titles]
                                   :from titles])))
           (edges (if org-roam-graph-exclude-matcher
                      (org-roam-sql [:select :distinct [file-to file-from]
                                     :from file-links
                                     :where file-to :not :like $s1
                                     :and file-from :not :like $s1]
                                    matcher)
                    (org-roam-sql [:select :distinct [file-to file-from]
                                   :from file-links]))))
	    (insert "digraph \"org-roam\" {\n")
      (dolist (option org-roam-graphviz-extra-options)
        (insert (concat (car option)
                        "="
                        (cdr option)
                        ";\n")))
      (dolist (node nodes)
        (let* ((file (xml-escape-string (car node)))
               (title (or (caadr node)
                          (org-roam--path-to-slug file)))
               (shortened-title (s-truncate org-roam-graph-max-title-length title)))
          (insert
		       (format "  \"%s\" [label=\"%s\", shape=%s, URL=\"org-protocol://roam-file?file=%s\", tooltip=\"%s\"];\n"
                   file
				           (xml-escape-string shortened-title)
				           org-roam-graph-node-shape
				           file
				           (xml-escape-string title)))))
      (dolist (edge edges)
        (insert (format "  \"%s\" -> \"%s\";\n"
                        (xml-escape-string (car edge))
						            (xml-escape-string (cadr edge)))))
	    (insert "}")
	    (buffer-string))))

(defun org-roam-show-graph (&optional prefix)
  "Generate and displays the Org-roam graph using `org-roam-graph-viewer'.
If PREFIX, then the graph is generated but the viewer is not invoked."
  (interactive "P")
  (declare (indent 0))
  (unless org-roam-graphviz-executable
    (setq org-roam-graphviz-executable (executable-find "dot")))
  (unless org-roam-graphviz-executable
    (user-error "Can't find graphviz executable.  Please check if it is in your path"))
  (let ((temp-dot (expand-file-name "graph.dot" temporary-file-directory))
        (temp-graph (expand-file-name "graph.svg" temporary-file-directory))
        (graph (org-roam--build-graph)))
    (with-temp-file temp-dot
      (insert graph))
    (call-process org-roam-graphviz-executable nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (unless prefix
      (if (and org-roam-graph-viewer (executable-find org-roam-graph-viewer))
	        (call-process org-roam-graph-viewer nil 0 nil temp-graph)
        (view-file temp-graph)))))

;;; The global minor org-roam-mode
(defvar org-roam-mode-map
  (make-sparse-keymap)
  "Keymap for mode `org-roam-mode'.")

;;;###autoload
(define-minor-mode org-roam-mode
  "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively."
  :lighter " Org-roam"
  :keymap  org-roam-mode-map
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (org-roam-build-cache)
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (add-hook 'kill-emacs-hook #'org-roam--db-close-all)
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice))
   (t
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (remove-hook 'kill-emacs-hook #'org-roam--db-close-all)
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    (org-roam--db-close-all)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (org-link-set-parameters "file" :face 'org-link)
        (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer t)
        (remove-hook 'after-save-hook #'org-roam--db-update-file t))))))

(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when mode `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam--maybe-update-buffer nil t)
    (add-hook 'after-save-hook #'org-roam--db-update-file nil t)
    (org-link-set-parameters "file" :face 'org-roam--roam-link-face)
    (org-roam--maybe-update-buffer :redisplay nil)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (org-roam--db-clear-file (file-truename file))))

(defun org-roam--rename-file-advice (file new-file &rest _args)
  "Rename backlinks of FILE to refer to NEW-FILE."
  (when (and (not (auto-save-file-name-p file))
             (not (auto-save-file-name-p new-file))
             (org-roam--org-roam-file-p new-file))
    (org-roam--db-ensure-built)
    (let* ((files-to-rename (org-roam-sql [:select :distinct [file-from]
                                           :from file-links
                                           :where (= file-to $s1)]
                                          file))
           (path (file-truename file))
           (new-path (file-truename new-file))
           (slug (org-roam--get-title-or-slug file))
           (old-title (org-roam--format-link-title slug))
           (new-slug (or (car (org-roam--db-get-titles path))
                         (org-roam--path-to-slug new-path)))
           (new-title (org-roam--format-link-title new-slug)))
      (org-roam--db-clear-file file)
      (dolist (file-from files-to-rename)
        (let* ((file-from (car file-from))
               (file-from (if (string-equal (file-truename file-from)
                                            path)
                              new-path
                            file-from))
               (file-dir (file-name-directory file-from))
               (relative-path (file-relative-name new-path file-dir))
               (old-relative-path (file-relative-name path file-dir))
               (slug-regex (regexp-quote (format "[[file:%s][%s]]" old-relative-path old-title)))
               (named-regex (concat
                             (regexp-quote (format "[[file:%s][" old-relative-path))
                             "\\(.*\\)"
                             (regexp-quote "]]"))))
          (with-temp-file file-from
            (insert-file-contents file-from)
            (while (re-search-forward slug-regex nil t)
              (replace-match (format "[[file:%s][%s]]" relative-path new-title)))
            (goto-char (point-min))
            (while (re-search-forward named-regex nil t)
              (replace-match (format "[[file:%s][\\1]]" relative-path))))
          (org-roam--db-update-file file-from)))
      (org-roam--db-update-file new-path))))
;;; -
(provide 'org-roam)
;;; org-roam.el ends here

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
