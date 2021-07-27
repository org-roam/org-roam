;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'ol)
(require 'org-capture)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 'seq)
(require 'magit-section)
(require 'emacsql)
(require 'emacsql-sqlite)

(require 'org-roam-migrate)
(require 'org-roam-compat)

(eval-when-compile
  (require 'subr-x)
  (require 'org-roam-macs)
  (require 'org-macs))

;;;; Declarations

(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
(declare-function org-datetree-find-date-create "org-datetree" (date &optional keep-restriction))
(declare-function org-datetree-find-month-create "org-datetree" (d &optional keep-restriction))

(defgroup org-roam nil
  "Roam Research replica in Org-mode."
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

(defcustom org-roam-file-extensions '("org")
  "Detected file extensions to include in the Org-roam ecosystem.
While a file extension different from \".org\" may be used, the
file still needs to be an `org-mode' file, and it is the user's
responsibility to ensure that."
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
              (const :tag "fdfind" fdfind)
              (const :tag "rg" rg)
              (const :tag "elisp" nil)))

(defvar org-roam-find-file-hook nil
  "Hook run when an Org-roam file is visited.")

;;;; Utility Functions
(defun org-roam-truncate-string (s len &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

;; TODO: Refactor this
(defun org-roam-replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun org-roam-quote-string (s)
  "Quotes string S."
  (->> s
    (org-roam-replace "\\" "\\\\")
    (org-roam-replace "\"" "\\\"")))

(defun org-roam--list-interleave (lst separator)
  "Interleaves elements in LST with SEPARATOR."
  (when lst
    (let ((new-lst (list (pop lst))))
      (dolist (it lst)
        (nconc new-lst (list separator it)))
      new-lst)))

(defun org-roam-up-heading-or-point-min ()
  "Fixed version of Org's `org-up-heading-or-point-min'."
  (ignore-errors (org-back-to-heading t))
  (let ((p (point)))
    (if (< 1 (funcall outline-level))
        (progn
          (org-up-heading-safe)
          (when (= (point) p)
            (goto-char (point-min))))
      (unless (bobp) (goto-char (point-min))))))

(defun org-roam-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `org-roam-verbose' is t."
  (when org-roam-verbose
    (apply #'message `(,(concat "(org-roam) " format-string) ,@args))))

(defvar org-ref-buffer-hacked)

(defun org-roam-fontify-like-in-org-mode (s)
  "Fontify string S like in Org mode.
Like `org-fontify-like-in-org-mode', but supports `org-ref'."
  ;; NOTE: pretend that the temporary buffer created by `org-fontify-like-in-org-mode' to
  ;; fontify a `cite:' reference has been hacked by org-ref, whatever that means;
  ;;
  ;; `org-ref-cite-link-face-fn', which is used to supply a face for `cite:' links, calls
  ;; `hack-dir-local-variables' rationalizing that `bibtex-completion' would throw some warnings
  ;; otherwise.  This doesn't seem to be the case and calling this function just before
  ;; `org-font-lock-ensure' (alias of `font-lock-ensure') actually instead of fixing the alleged
  ;; warnings messes the things so badly that `font-lock-ensure' crashes with error and doesn't let
  ;; org-roam to proceed further. I don't know what's happening there exactly but disabling this hackery
  ;; fixes the crashing.  Fortunately, org-ref provides the `org-ref-buffer-hacked' switch, which we use
  ;; here to make it believe that the buffer was hacked.
  ;;
  ;; This is a workaround for `cite:' links and does not have any effect on other ref types.
  ;;
  ;; `org-ref-buffer-hacked' is a buffer-local variable, therefore we inline
  ;; `org-fontify-like-in-org-mode' here
  (with-temp-buffer
    (insert s)
    (let ((org-ref-buffer-hacked t))
      (org-mode)
      (org-font-lock-ensure)
      (buffer-string))))

(defun org-roam-set-header-line-format (string)
  "Set the header-line using STRING.
If the `face' property of any part of STRING is already set, then
that takes precedence. Also pad the left side of STRING so that
it aligns with the text area."
  (setq-local header-line-format
              (concat (propertize " " 'display '(space :align-to 0))
                      string)))

(defun org-roam--get-keyword (name &optional bound)
  "Return keyword property NAME in current buffer.
If BOUND, scan up to BOUND bytes of the buffer."
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun org-roam-get-keyword (name &optional file bound)
  "Return keyword property NAME from an org FILE.
FILE defaults to current file.
Only scans up to BOUND bytes of the document."
  (unless bound
    (setq bound 1024))
  (if file
      (with-temp-buffer
        (insert-file-contents file nil 0 bound)
        (org-roam--get-keyword name))
    (org-roam--get-keyword name bound)))

(defface org-roam-shielded
  '((t :inherit (warning)))
  "Face for regions that are shielded (marked as read-only).
This face is used on the region target by org-roam-insertion
during an `org-roam-capture'."
  :group 'org-roam-faces)

(defun org-roam-shield-region (beg end)
  "Shield region against modifications.
BEG and END are markers for the beginning and end regions.
REGION must be a cons-cell containing the marker to the region
beginning and maximum values."
  (add-text-properties beg end
                       '(font-lock-face org-roam-shielded
                                        read-only t)
                       (marker-buffer beg)))

(defun org-roam-unshield-region (beg end)
  "Unshield the shielded REGION.
BEG and END are markers for the beginning and end regions."
  (let ((inhibit-read-only t))
    (remove-text-properties beg end
                            '(font-lock-face org-roam-shielded
                                             read-only t)
                            (marker-buffer beg))))

(defun org-roam-format (template replacer)
  "Format TEMPLATE with the function REPLACER.
REPLACER takes an argument of the format variable and optionally
an extra argument which is the EXTRA value from the call to
`org-roam-format'.
Adapted from `s-format'."
  (let ((saved-match-data (match-data)))
    (unwind-protect
        (replace-regexp-in-string
         "\\${\\([^}]+\\)}"
         (lambda (md)
           (let ((var (match-string 1 md))
                 (replacer-match-data (match-data)))
             (unwind-protect
                 (let ((v (progn
                            (set-match-data saved-match-data)
                            (funcall replacer var))))
                   (if v (format "%s" v) (signal 'org-roam-format-resolve md)))
               (set-match-data replacer-match-data)))) template
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

(defvar org-roam--cached-display-format nil)

(defun org-roam--process-display-format (format)
  "Pre-calculate minimal widths needed by the FORMAT string."
  (or org-roam--cached-display-format
      (setq org-roam--cached-display-format
            (let* ((fields-width 0)
                   (string-width
                    (string-width
                     (org-roam-format
                      format
                      (lambda (field)
                        (setq fields-width
                              (+ fields-width
                                 (string-to-number
                                  (or (cadr (split-string field ":"))
                                      "")))))))))
              (cons format (+ fields-width string-width))))))

(defun org-roam--file-keyword-get (keyword)
  "Pull a KEYWORD setting from the top of the file.
Keyword must be specified in ALL CAPS."
  (cadr (assoc keyword
               (org-collect-keywords (list keyword)))))

(defun org-roam--file-keyword-kill (keyword)
  "Erase KEYWORD setting line from the top of the file."
  (let ((case-fold-search t))
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" keyword ":") nil t)
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (delete-char 1)))))

(defun org-roam--kill-empty-buffer ()
  "If the source buffer has been emptied, kill it.

If the buffer is associated with a file, delete the file.

If the buffer is associated with an in-process capture operation, abort the operation."
  (when (eq (buffer-size) 0)
    (if (buffer-file-name)
        (delete-file (buffer-file-name)))
    (set-buffer-modified-p nil)
    (when (and org-capture-mode
               (buffer-base-buffer (current-buffer)))
      (org-capture-kill))
    (kill-buffer (current-buffer))))

;; TODO: Update this to also get commit hash
;;;###autoload
(defun org-roam-version (&optional message)
  "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((version
          (with-temp-buffer
            (insert-file-contents-literally (locate-library "org-roam.el"))
            (goto-char (point-min))
            (save-match-data
              (if (re-search-forward "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                  (substring-no-properties (match-string 1))
                "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

;;;###autoload
(defun org-roam-diagnostics ()
  "Collect and print info for `org-roam' issues."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*org-roam diagnostics*"))
    (erase-buffer)
    (insert (propertize "Copy info below this line into issue:\n" 'face '(:weight bold)))
    (insert (format "- Emacs: %s\n" (emacs-version)))
    (insert (format "- Framework: %s\n"
                    (condition-case _
                        (completing-read "I'm using the following Emacs framework:"
                                         '("Doom" "Spacemacs" "N/A" "I don't know"))
                      (quit "N/A"))))
    (insert (format "- Org: %s\n" (org-version nil 'full)))
    (insert (format "- Org-roam: %s" (org-roam-version)))))

;;;; The Org-roam Database
(defcustom org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
  "The path where the Org-roam database is stored.
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

;; TODO: rewrite help message
(defcustom org-roam-db-node-include-function (lambda () t)
  "A custom function to check if the headline at point is a node."
  :type 'function
  :group 'org-roam)

(defconst org-roam-db-version 16)

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

(defconst org-roam--sqlite-available-p
  (with-demoted-errors "Org-roam initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

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

(defun org-roam-db-insert-node-data ()
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
           (title (org-link-display-format (nth 4 heading-components)))
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

;;;###autoload
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

;;;; org-roam-capture
(defvar org-roam-capture--node nil
  "The node passed during an Org-roam capture.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam-capture--info nil
  "A property-list of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defconst org-roam-capture--template-keywords (list :if-new :id :link-description :call-location
                                                    :region :override-default-time)
  "Keywords used in `org-roam-capture-templates' specific to Org-roam.")

(defcustom org-roam-capture-templates
  '(("d" "default" plain "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t))
  "Templates for the creation of new entries within Org-roam.

Each entry is a list with the following items:

keys   The keys that will select the template, as a string, characters only, for
       example \"a\" for a template to be selected with a single key, or
       \"bt\" for selection with two keys. When using several keys, keys
       using the same prefix must be together in the list and preceded by a
       2-element entry explaining the prefix key, for example:

                   (\"b\" \"Templates for marking stuff to buy\")

       The \"C\" key is used by default for quick access to the customization of
       the template variable. But if you want to use that key for a template,
       you can.

description   A short string describing the template, which will be shown
              during selection.

type       The type of entry. Valid types are:
               entry       an Org node, with a headline.  Will be filed
                           as the child of the target entry or as a
                           top level entry.  Its default template is:
                             \"* %?\n %a\"
               item        a plain list item, will be placed in the
                           first plain list at the target location.
                           Its default template is:
                             \"- %?\"
               checkitem   a checkbox item.  This differs from the
                           plain list item only in so far as it uses a
                           different default template.  Its default
                           template is:
                             \"- [ ] %?\"
               table-line  a new line in the first table at target location.
                           Its default template is:
                             \"| %? |\"
               plain       text to be inserted as it is.

template     The template for creating the capture item.
             If it is an empty string or nil, a default template based on
             the entry type will be used (see the \"type\" section above).
             Instead of a string, this may also be one of:

                 (file \"/path/to/template-file\")
                 (function function-returning-the-template)

             in order to get a template from a file, or dynamically
             from a function.

The template contains a compulsory :if-new property. This determines the
location of the new node. The :if-new property contains a list, supporting
the following options:

   (file \"path/to/file\")
       The file will be created, and prescribed an ID.

   (file+head \"path/to/file\" \"head content\")
       The file will be created, prescribed an ID, and head content will be
       inserted into the file.

   (file+olp \"path/to/file\" (\"h1\" \"h2\"))
       The file will be created, prescribed an ID. The OLP (h1, h2) will be
       created, and the point placed after.

   (file+head+olp \"path/to/file\" \"head content\" (\"h1\" \"h2\"))
       The file will be created, prescribed an ID. Head content will be
       inserted at the start of the file. The OLP (h1, h2) will be created,
       and the point placed after.

   (file+datetree \"path/to/file\" day)
       The file will be created, prescribed an ID. Head content will be
       inserted at the start of the file. The datetree will be created,
       available options are day, week, month.

The rest of the entry is a property list of additional options.  Recognized
properties are:

 :prepend            Normally newly captured information will be appended at
                     the target location (last child, last table line,
                     last list item...).  Setting this property will
                     change that.

 :immediate-finish   When set, do not offer to edit the information, just
                     file it away immediately.  This makes sense if the
                     template only needs information that can be added
                     automatically.

 :jump-to-captured   When set, jump to the captured entry when finished.

 :empty-lines        Set this to the number of lines that should be inserted
                     before and after the new item.  Default 0, only common
                     other value is 1.

 :empty-lines-before Set this to the number of lines that should be inserted
                     before the new item.  Overrides :empty-lines for the
                     number lines inserted before.

 :empty-lines-after  Set this to the number of lines that should be inserted
                     after the new item.  Overrides :empty-lines for the
                     number of lines inserted after.

 :clock-in           Start the clock in this item.

 :clock-keep         Keep the clock running when filing the captured entry.

 :clock-resume       Start the interrupted clock when finishing the capture.
                     Note that :clock-keep has precedence over :clock-resume.
                     When setting both to t, the current clock will run and
                     the previous one will not be resumed.

 :time-prompt        Prompt for a date/time to be used for date/week trees
                     and when filling the template.

 :tree-type          When `week', make a week tree instead of the month-day
                     tree.  When `month', make a month tree instead of the
                     month-day tree.

 :unnarrowed         Do not narrow the target buffer, simply show the
                     full buffer.  Default is to narrow it so that you
                     only see the new stuff.

 :table-line-pos     Specification of the location in the table where the
                     new line should be inserted.  It should be a string like
                     \"II-3\", meaning that the new line should become the
                     third line before the second horizontal separator line.

 :kill-buffer        If the target file was not yet visited by a buffer when
                     capture was invoked, kill the buffer again after capture
                     is finalized.

 :no-save            Do not save the target file after finishing the capture.

The template defines the text to be inserted.  Often this is an
Org mode entry (so the first line should start with a star) that
will be filed as a child of the target headline.  It can also be
freely formatted text.  Furthermore, the following %-escapes will
be replaced with content and expanded:

  %[pathname] Insert the contents of the file given by
              `pathname'.  These placeholders are expanded at the very
              beginning of the process so they can be used to extend the
              current template.
  %(sexp)     Evaluate elisp `(sexp)' and replace it with the results.
              Only placeholders pre-existing within the template, or
              introduced with %[pathname] are expanded this way.  Since this
              happens after expanding non-interactive %-escapes, those can
              be used to fill the expression.
  %<...>      The result of `format-time-string' on the ... format specification.
  %t          Time stamp, date only.  The time stamp is the current time,
              except when called from agendas with `\\[org-agenda-capture]' or
              with `org-capture-use-agenda-date' set.
  %T          Time stamp as above, with date and time.
  %u, %U      Like the above, but inactive time stamps.
  %i          Initial content, copied from the active region.  If
              there is text before %i on the same line, such as
              indentation, and %i is not inside a %(sexp), that prefix
              will be added before every line in the inserted text.
  %a          Annotation, normally the link created with `org-store-link'.
  %A          Like %a, but prompt for the description part.
  %l          Like %a, but only insert the literal link.
  %L          Like %l, but without brackets (the link content itself).
  %c          Current kill ring head.
  %x          Content of the X clipboard.
  %k          Title of currently clocked task.
  %K          Link to currently clocked task.
  %n          User name (taken from the variable `user-full-name').
  %f          File visited by current buffer when `org-capture' was called.
  %F          Full path of the file or directory visited by current buffer.
  %:keyword   Specific information for certain link types, see below.
  %^g         Prompt for tags, with completion on tags in target file.
  %^G         Prompt for tags, with completion on all tags in all agenda files.
  %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
              You may define a prompt like: %^{Please specify birthday}t.
              The default date is that of %t, see above.
  %^C         Interactive selection of which kill or clip to use.
  %^L         Like %^C, but insert as link.
  %^{prop}p   Prompt the user for a value for property `prop'.
              A default value can be specified like this:
              %^{prop|default}p.
  %^{prompt}  Prompt the user for a string and replace this sequence with it.
              A default value and a completion table can be specified like this:
              %^{prompt|default|completion2|completion3|...}.
  %?          After completing the template, position cursor here.
  %\\1 ... %\\N Insert the text entered at the nth %^{prompt}, where N
              is a number, starting from 1.

Apart from these general escapes, you can access information specific to
the link type that is created.  For example, calling `org-capture' in emails
or in Gnus will record the author and the subject of the message, which you
can access with \"%:from\" and \"%:subject\", respectively.  Here is a
complete list of what is recorded for each link type.

Link type               |  Available information
------------------------+------------------------------------------------------
bbdb                    |  %:type %:name %:company
vm, wl, mh, mew, rmail, |  %:type %:subject %:message-id
gnus                    |  %:from %:fromname %:fromaddress
                        |  %:to   %:toname   %:toaddress
                        |  %:fromto (either \"to NAME\" or \"from NAME\")
                        |  %:date %:date-timestamp (as active timestamp)
                        |  %:date-timestamp-inactive (as inactive timestamp)
gnus                    |  %:group, for messages also all email fields
eww, w3, w3m            |  %:type %:url
info                    |  %:type %:file %:node
calendar                |  %:type %:date

When you need to insert a literal percent sign in the template,
you can escape ambiguous cases with a backward slash, e.g., \\%i.

In addition to all of the above, Org-roam supports additional
substitutions within its templates. \"${foo}\" will look for the
foo property in the Org-roam node (see the `org-roam-node'). If
the property does not exist, the user will be prompted to fill in
the string value.

Org-roam templates are NOT compatible with regular Org capture:
they rely on additional hacks and hooks to achieve the
streamlined user experience in Org-roam."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :if-new)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

(defvar org-roam-capture-new-node-hook (list #'org-roam-capture--insert-ref)
  "Normal-mode hooks run when a new Org-roam node is created.
The current point is the point of the new node.
The hooks must not move the point.")

(defcustom org-roam-capture-ref-templates
  '(("r" "ref" plain "%?"
     :if-new (file+head "${slug}.org"
                        "#+title: ${title}")
     :unnarrowed t))
  "The Org-roam templates used during a capture from the roam-ref protocol.
See `org-roam-capture-templates' for the template documentation."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :if-new)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

(defun org-roam-capture-p ()
  "Return t if the current capture process is an Org-roam capture.
This function is to only be called when `org-capture-plist' is
valid for the capture (i.e. initialization, and finalization of
the capture)."
  (plist-get org-capture-plist :org-roam))

(defun org-roam-capture--get (keyword)
  "Get the value for KEYWORD from the `org-roam-capture-template'."
  (plist-get (plist-get org-capture-plist :org-roam) keyword))

(defun org-roam-capture--put (&rest stuff)
  "Put properties from STUFF into the `org-roam-capture-template'."
  (let ((p (plist-get org-capture-plist :org-roam)))
    (while stuff
      (setq p (plist-put p (pop stuff) (pop stuff))))
    (setq org-capture-plist
          (plist-put org-capture-plist :org-roam p))))

;; FIXME: Pending upstream patch
;; https://orgmode.org/list/87h7tv9pkm.fsf@hidden/T/#u
;;
;; Org-capture's behaviour right now is that `org-capture-plist' is valid only
;; during the initialization of the Org-capture buffer. The value of
;; `org-capture-plist' is saved into buffer-local `org-capture-current-plist'.
;; However, the value for that particular capture is no longer accessible for
;; hooks in `org-capture-after-finalize-hook', since the capture buffer has been
;; cleaned up.
;;
;; This advice restores the global `org-capture-plist' during finalization, so
;; the plist is valid during both initialization and finalization of the
;; capture.
(defun org-roam-capture--update-plist (&optional _)
  "Update global plist from local var."
  (setq org-capture-plist org-capture-current-plist))

(advice-add 'org-capture-finalize :before #'org-roam-capture--update-plist)

(defun org-roam-capture--finalize-find-file ()
  "Visit the buffer after Org-capture is done.
This function is to be called in the Org-capture finalization process.
ID is unused."
  (switch-to-buffer (org-capture-get :buffer)))

(defun org-roam-capture--finalize-insert-link ()
  "Insert a link to ID into the buffer where Org-capture was called.
ID is the Org id of the newly captured content.
This function is to be called in the Org-capture finalization process."
  (when-let* ((mkr (org-roam-capture--get :call-location))
              (buf (marker-buffer mkr)))
    (with-current-buffer buf
      (when-let ((region (org-roam-capture--get :region)))
        (org-roam-unshield-region (car region) (cdr region))
        (delete-region (car region) (cdr region))
        (set-marker (car region) nil)
        (set-marker (cdr region) nil))
      (org-with-point-at mkr
        (insert (org-link-make-string (concat "id:" (org-roam-capture--get :id))
                                      (org-roam-capture--get :link-description)))))))

(defun org-roam-capture--finalize ()
  "Finalize the `org-roam-capture' process."
  (when-let ((region (org-roam-capture--get :region)))
    (org-roam-unshield-region (car region) (cdr region)))
  (if org-note-abort
      (when-let ((new-file (org-roam-capture--get :new-file)))
        (org-roam-message "Deleting file for aborted capture %s" new-file)
        (when (find-buffer-visiting new-file)
          (kill-buffer (find-buffer-visiting new-file)))
        (delete-file new-file))
    (when-let* ((finalize (org-roam-capture--get :finalize))
                (org-roam-finalize-fn (intern (concat "org-roam-capture--finalize-"
                                                      (symbol-name finalize)))))
      (if (functionp org-roam-finalize-fn)
          (funcall org-roam-finalize-fn)
        (funcall finalize))))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize))

(defun org-roam-capture--install-finalize ()
  "Install `org-roam-capture--finalize' if the capture is an Org-roam capture."
  (when (org-roam-capture-p)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize)))

(add-hook 'org-capture-prepare-finalize-hook #'org-roam-capture--install-finalize)

(defun org-roam-capture--fill-template (template &optional org-capture-p)
  "Expand TEMPLATE and return it.
It expands ${var} occurrences in TEMPLATE. When ORG-CAPTURE-P,
also run Org-capture's template expansion."
  (funcall (if org-capture-p #'org-capture-fill-template #'identity)
           (org-roam-format
            template
            (lambda (key)
              (let ((fn (intern key))
                    (node-fn (intern (concat "org-roam-node-" key)))
                    (ksym (intern (concat ":" key))))
                (cond
                 ((fboundp fn)
                  (funcall fn org-roam-capture--node))
                 ((fboundp node-fn)
                  (funcall node-fn org-roam-capture--node))
                 ((plist-get org-roam-capture--info ksym)
                  (plist-get org-roam-capture--info ksym))
                 (t (let ((r (completing-read (format "%s: " key) nil)))
                      (plist-put org-roam-capture--info ksym r)
                      r))))))))

(defun org-roam-capture--insert-ref ()
  "Insert the ref if any."
  (when-let ((ref (plist-get org-roam-capture--info :ref)))
    (org-roam-ref-add ref)))

(defun org-roam-capture--goto-location ()
  "Initialize the buffer, and goto the location of the new capture.
Return the ID of the location."
  (let (p)
    (pcase (or (org-roam-capture--get :if-new)
               (user-error "Template needs to specify `:if-new'"))
      (`(file ,path)
       (setq path (expand-file-name
                   (string-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+olp ,path ,olp)
       (setq path (expand-file-name
                   (string-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m))
       (widen))
      (`(file+head ,path ,head)
       (setq path (expand-file-name
                   (string-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head t)))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+head+olp ,path ,head ,olp)
       (setq path (expand-file-name
                   (string-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (widen)
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head t)))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m)))
      (`(file+datetree ,path ,tree-type)
       (setq path (expand-file-name
                   (string-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (require 'org-datetree)
       (widen)
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (funcall
        (pcase tree-type
          (`week #'org-datetree-find-iso-week-create)
          (`month #'org-datetree-find-month-create)
          (_ #'org-datetree-find-date-create))
        (calendar-gregorian-from-absolute
         (cond
          (org-overriding-default-time
           ;; Use the overriding default time.
           (time-to-days org-overriding-default-time))
          ((org-capture-get :default-time)
           (time-to-days (org-capture-get :default-time)))
          ((org-capture-get :time-prompt)
           ;; Prompt for date.  Bind `org-end-time-was-given' so
           ;; that `org-read-date-analyze' handles the time range
           ;; case and returns `prompt-time' with the start value.
           (let* ((org-time-was-given nil)
                  (org-end-time-was-given nil)
                  (prompt-time (org-read-date
                                nil t nil "Date for tree entry:")))
             (org-capture-put
              :default-time
              (if (or org-time-was-given
                      (= (time-to-days prompt-time) (org-today)))
                  prompt-time
                ;; Use 00:00 when no time is given for another
                ;; date than today?
                (apply #'encode-time 0 0
                       org-extend-today-until
                       (cl-cdddr (decode-time prompt-time)))))
             (time-to-days prompt-time)))
          (t
           ;; Current date, possibly corrected for late night
           ;; workers.
           (org-today)))))
       (setq p (point)))
      (`(node ,title-or-id)
       ;; first try to get ID, then try to get title/alias
       (let ((node (or (org-roam-node-from-id title-or-id)
                       (org-roam-node-from-title-or-alias title-or-id)
                       (user-error "No node with title or id \"%s\" title-or-id"))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node)))))
    ;; Setup `org-id' for the current capture target and return it back to the
    ;; caller.
    (save-excursion
      (goto-char p)
      (when-let* ((node org-roam-capture--node)
                  (id (org-roam-node-id node)))
        (org-entry-put p "ID" id))
      (prog1
          (org-id-get-create)
        (run-hooks 'org-roam-capture-new-node-hook)))))

(defun org-roam-capture--adjust-point-for-capture-type (&optional pos)
  "Reposition the point for template insertion dependently on the capture type.
Return the newly adjusted position of `point'.

POS is the current position of point (an integer) inside the
currently active capture buffer, where the adjustment should
start to begin from. If it's nil, then it will default to
the current value of `point'."
  (or pos (setq pos (point)))
  (goto-char pos)
  (let ((location-type (if (= pos 1) 'beginning-of-file 'heading-at-point)))
    (and (eq location-type 'heading-at-point)
         (cl-assert (org-at-heading-p)))
    (pcase (org-capture-get :type)
      (`plain
       (cl-case location-type
         (beginning-of-file
          (if (org-capture-get :prepend)
              (let ((el (org-element-at-point)))
                (while (and (not (eobp))
                            (memq (org-element-type el)
                                  '(drawer property-drawer keyword comment comment-block horizontal-rule)))
                  (goto-char (org-element-property :end el))
                  (setq el (org-element-at-point))))
            (goto-char (org-entry-end-position))))
         (heading-at-point
          (if (org-capture-get :prepend)
              (org-end-of-meta-data t)
            (goto-char (org-entry-end-position))))))))
  (point))

(defun org-roam-capture-find-or-create-olp (olp)
  "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
  (let* ((level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading olp)
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (let (org-insert-heading-respect-content)
             (org-insert-heading nil nil t))
           (unless (= lmax 1)
             (dotimes (_ level) (org-do-demote)))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

(defun org-roam-capture--get-node-from-ref (ref)
  "Return the node from reference REF."
  (save-match-data
    (when (string-match org-link-plain-re ref)
      (let ((type (match-string 1 ref))
            (path (match-string 2 ref)))
        (when-let ((id (caar (org-roam-db-query
                              [:select [nodes:id]
                               :from refs
                               :left-join nodes
                               :on (= refs:node-id nodes:id)
                               :where (= refs:type $s1)
                               :and (= refs:ref $s2)
                               :limit 1]
                              type path))))
          (org-roam-populate (org-roam-node-create :id id)))))))

(defun org-roam-capture--get-point ()
  "Return exact point to file for org-capture-template.
This function is used solely in Org-roam's capture templates: see
`org-roam-capture-templates'."
  (when (org-roam-capture--get :override-default-time)
    (org-capture-put :default-time (org-roam-capture--get :override-default-time)))
  (let ((id (cond ((plist-get org-roam-capture--info :ref)
                   (if-let ((node (org-roam-capture--get-node-from-ref
                                   (plist-get org-roam-capture--info :ref))))
                       (progn
                         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
                         (goto-char (org-roam-node-point node))
                         (widen))
                     (org-roam-capture--goto-location)))
                  ((and (org-roam-node-file org-roam-capture--node)
                        (org-roam-node-point org-roam-capture--node))
                   (set-buffer (org-capture-target-buffer (org-roam-node-file org-roam-capture--node)))
                   (goto-char (org-roam-node-point org-roam-capture--node))
                   (widen)
                   (org-roam-node-id org-roam-capture--node))
                  (t
                   (org-roam-capture--goto-location)))))
    (org-roam-capture--adjust-point-for-capture-type)
    (org-capture-put :template
                     (org-roam-capture--fill-template (org-capture-get :template)))
    (org-roam-capture--put :id id)
    (org-roam-capture--put :finalize (or (org-capture-get :finalize)
                                         (org-roam-capture--get :finalize)))))

(defun org-roam-capture--convert-template (template &optional props)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax.
PROPS is a plist containing additional Org-roam specific
properties to be added to the template."
  (pcase template
    (`(,_key ,_desc)
     template)
    (`(,key ,desc ,type ,body . ,rest)
     (setq rest (append rest props))
     (let (org-roam-plist options)
       (while rest
         (let* ((key (pop rest))
                (val (pop rest))
                (custom (member key org-roam-capture--template-keywords)))
           (when (and custom
                      (not val))
             (user-error "Invalid capture template format: %s\nkey %s cannot be nil" template key))
           (if custom
               (setq org-roam-plist (plist-put org-roam-plist key val))
             (setq options (plist-put options key val)))))
       (append `(,key ,desc ,type #'org-roam-capture--get-point ,body)
               options
               (list :org-roam org-roam-plist))))
    (_
     (signal 'invalid-template template))))

;;;###autoload
(cl-defun org-roam-capture- (&key goto keys node info props templates)
  "Main entry point.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is an alist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates."
  (let* ((props (plist-put props :call-location (point-marker)))
         (org-capture-templates
          (mapcar (lambda (template)
                    (org-roam-capture--convert-template template props))
                  (or templates org-roam-capture-templates)))
         (org-roam-capture--node node)
         (org-roam-capture--info info))
    (when (and (not keys)
               (= (length org-capture-templates) 1))
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)))

;;;###autoload
(defun org-roam-capture (&optional goto keys)
  "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'."
  (interactive "P")
  (let ((node (org-roam-node-read)))
    (org-roam-capture- :goto goto
                       :keys keys
                       :node node
                       :props '(:immediate-finish nil))))

;;;; Org-roam-mode
(defvar org-roam-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'org-roam-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-visit-thing)
    (define-key map [remap revert-buffer] 'org-roam-buffer-refresh)
    map)
  "Parent keymap for all keymaps of modes derived from `org-roam-mode'.")

(define-derived-mode org-roam-mode magit-section-mode "Org-roam"
  "Major mode for Org-roam's buffer."
  :group 'org-roam
  (face-remap-add-relative 'header-line 'org-roam-header-line))

;;;; Core

(defun org-roam-id-at-point ()
  "Return the ID at point, if any.
Recursively traverses up the headline tree to find the
first encapsulating ID."
  (org-with-wide-buffer
   (org-back-to-heading-or-point-min)
   (while (and (not (org-roam-db-node-p))
               (not (bobp)))
     (org-roam-up-heading-or-point-min))
   (org-id-get)))

;;;; File functions and predicates
(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let* ((path (or file (buffer-file-name (buffer-base-buffer))))
         (ext (when path (org-roam--file-name-extension path)))
         (ext (if (string= ext "gpg")
                  (org-roam--file-name-extension (file-name-sans-extension path))
                ext)))
    (save-match-data
      (and
       path
       (member ext org-roam-file-extensions)
       (not (and org-roam-file-exclude-regexp
                 (string-match-p org-roam-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-directory))))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (--> cmd
       (shell-command-to-string it)
       (ansi-color-filter-apply it)
       (split-string it "\n")
       (seq-filter #'s-present? it)))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (cl-loop for e in exts
           append (list (format "\"*.%s\"" e)
                        (format "\"*.%s.gpg\"" e))))

(defun org-roam--list-files-fd (executable dir)
  "Return all Org-roam files located recursively within DIR, using fd, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (extensions (s-join " -e " (mapcar (lambda (glob) (substring glob 2 -1)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "--type file" ,extensions))))
    (org-roam--shell-command-files command)))

(defalias 'org-roam--list-files-fdfind #'org-roam--list-files-fd)

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (names (s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\(" ,names "\\)"))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat
                                  #'regexp-quote org-roam-file-extensions
                                  "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (org-roam--directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file)
                 (org-roam-file-p file))
        (push file result)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

(defun org-roam--nodes-table ()
  "Return a hash table of node ID to org-roam-nodes."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,file ,title)
                   (org-roam-db-query [:select [id file title] :from nodes]))
      (puthash id (org-roam-node-create :file file :id id :title title) ht))
    ht))

(defun org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of Org-roam system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam-file-p path)))))

(defun org-roam-buffer-list ()
  "Return a list of buffers that are Org-roam files."
  (--filter (org-roam-buffer-p it)
            (buffer-list)))

(defun org-roam--get-titles ()
  "Return all distinct titles and aliases in the Org-roam database."
  (mapcar #'car (org-roam-db-query [:select :distinct title :from nodes
                                    :union :select alias :from aliases])))

;;;; Setup and teardown
;;;###autoload
(defun org-roam-setup ()
  "Setup Org-roam and initialize its database.
This will install the needed hooks and advices to keep everything
in sync with the connected databases."
  (interactive)
  (add-hook 'find-file-hook #'org-roam--file-setup)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-add 'rename-file :after #'org-roam--rename-file-advice)
  (advice-add 'delete-file :before #'org-roam--delete-file-advice)
  (org-roam-db-sync))

(defun org-roam-teardown ()
  "Teardown Org-roam to completely disable it.
This will remove all the hooks and advices installed by
`org-roam-setup' and close all the database connections made by
Org-roam."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam--file-setup)
  (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-remove 'rename-file #'org-roam--rename-file-advice)
  (advice-remove 'delete-file #'org-roam--delete-file-advice)
  (org-roam-db--close-all)
  ;; Disable local hooks for all org-roam buffers
  (dolist (buf (org-roam-buffer-list))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-db-update-file t))))

;;;; Hooks and advices
(defun org-roam--file-setup ()
  "Setup an Org-roam file."
  (when (org-roam-file-p)
    (run-hooks 'org-roam-find-file-hook)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-file-p file))
    (org-roam-db-clear-file (expand-file-name file))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
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

;;;; Nodes
(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
                             (:copier nil))
  "A heading or top level file with an assigned ID property."
  file file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs)

(cl-defmethod org-roam-node-slug ((node org-roam-node))
  "Return the slug of NODE."
  (let ((title (org-roam-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                      ("__*" . "_")  ;; remove sequential underscores
                      ("^_" . "")    ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-node-visit)
    map)
  "Keymap for `org-roam-node-section's.")

(defclass org-roam-node-section (magit-section)
  ((keymap :initform 'org-roam-node-map)
   (node :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain heading for NODE.")

(defvar org-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-preview-visit)
    map)
  "Keymap for `org-roam-preview-section's.")

(defclass org-roam-preview-section (magit-section)
  ((keymap :initform 'org-roam-preview-map)
   (file :initform nil)
   (point :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, and the link as at POINT.")

(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (when-let ((node-info (car (org-roam-db-query [:select [file level pos todo priority
                                                          scheduled deadline title properties olp]
                                                 :from nodes
                                                 :where (= id $s1)
                                                 :limit 1]
                                                (org-roam-node-id node)))))
    (pcase-let* ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title ,properties ,olp) node-info)
                 (`(,atime ,mtime) (car (org-roam-db-query [:select [atime mtime]
                                                            :from files
                                                            :where (= file $s1)]
                                                           file)))
                 (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                             :where (= node-id $s1)]
                                                            (org-roam-node-id node))))
                 (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                               :where (= node-id $s1)]
                                                              (org-roam-node-id node))))
                 (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                              :where (= node-id $s1)]
                                                             (org-roam-node-id node)))))
      (setf (org-roam-node-file node) file
            (org-roam-node-file-atime node) atime
            (org-roam-node-file-mtime node) mtime
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-properties node) properties
            (org-roam-node-olp node) olp
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info)))
  node)

(defcustom org-roam-node-display-template
  "${title:*} ${tags:10}"
  "Configures display formatting for Org-roam node.
Patterns of form \"${field-name:length}\" are interpolated based
on the current node.

Each \"field-name\" is replaced with the return value of each
corresponding accessor function for `org-roam-node', e.g.
\"${title}\" will be interpolated by the result of
`org-roam-node-title'. You can also define custom accessors using
`cl-defmethod'. For example, you can define:

  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (concat \"My \" (org-roam-node-title node)))

and then reference it here or in the capture templates as
\"${my-title}\".

\"length\" is an optional specifier and declares how many
characters can be used to display the value of the corresponding
field. If it's not specified, the field will be inserted as is,
i.e. it won't be aligned nor trimmed. If it's an integer, the
field will be aligned accordingly and all the exceeding
characters will be trimmed out. If it's \"*\", the field will use
as many characters as possible and will be aligned accordingly."
  :group 'org-roam
  :type  'string)

(defun org-roam--tags-to-str (tags)
  "Convert list of TAGS into a string."
  (mapconcat (lambda (s) (concat "#" s)) tags " "))

(defun org-roam-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
Uses `org-roam-node-display-template' to format the entry."
  (let ((fmt (org-roam--process-display-format org-roam-node-display-template)))
    (org-roam-format
     (car fmt)
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cadr field))
              (getter (intern (concat "org-roam-node-" field-name)))
              (field-value (or (funcall getter node) "")))
         (when (and (equal field-name "tags")
                    field-value)
           (setq field-value (org-roam--tags-to-str field-value)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (file-relative-name field-value org-roam-directory)))
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (if (not field-width)
             field-value
           (setq field-width (string-to-number field-width))
           (truncate-string-to-width
            field-value
            (if (> field-width 0)
                field-width
              (- width (cdr fmt)))
            0 ?\s)))))))

(defun org-roam-get-preview (file point)
  "Get preview content for FILE at POINT."
  (save-excursion
    (org-roam-with-temp-buffer file
      (goto-char point)
      (let ((elem (org-element-at-point)))
        ;; We want the parent element always
        (while (org-element-property :parent elem)
          (setq elem (org-element-property :parent elem)))
        (pcase (car elem)
          ('headline                    ; show subtree
           (org-roam-headline-get-preview-text (point-marker) most-positive-fixnum))
          (_
           (let ((begin (org-element-property :begin elem))
                 (end (org-element-property :end elem)))
             (or (string-trim (buffer-substring-no-properties begin end))
                 (org-element-property :raw-value elem)))))))))

(defun org-roam-headline-get-preview-text (marker n-lines &optional indent)
  "Extract entry text from MARKER, at most N-LINES lines.
This will ignore drawers etc, just get the text.
If INDENT is given, prefix every line with this string."
  (let (txt drawer-re kwd-time-re ind)
    (save-excursion
      (with-current-buffer (marker-buffer marker)
        (if (not (derived-mode-p 'org-mode))
            (setq txt "")
          (org-with-wide-buffer
           (goto-char marker)
           (end-of-line 1)
           (setq txt (buffer-substring
                      (min (1+ (point)) (point-max))
                      (progn (outline-next-heading) (point))))
           (with-temp-buffer
             (insert txt)
             (goto-char (point-min))
             (while (org-activate-links (point-max))
               (goto-char (match-end 0)))
             (goto-char (point-min))
             (while (re-search-forward org-link-bracket-re (point-max) t)
               (set-text-properties (match-beginning 0) (match-end 0)
                                    nil))
             (goto-char (point-min))
             (while (re-search-forward org-drawer-regexp nil t)
               (delete-region
                (match-beginning 0)
                (progn (re-search-forward
                        "^[ \t]*:END:.*\n?" nil 'move)
                       (point))))
             (goto-char (point-min))
             (goto-char (point-max))
             (skip-chars-backward " \t\n")
             (when (looking-at "[ \t\n]+\\'") (replace-match ""))

             ;; find and remove min common indentation
             (goto-char (point-min))
             (untabify (point-min) (point-max))
             (setq ind (current-indentation))
             (while (not (eobp))
               (unless (looking-at "[ \t]*$")
                 (setq ind (min ind (current-indentation))))
               (beginning-of-line 2))
             (goto-char (point-min))
             (while (not (eobp))
               (unless (looking-at "[ \t]*$")
                 (move-to-column ind)
                 (delete-region (point-at-bol) (point)))
               (beginning-of-line 2))
             (goto-char (point-min))
             (when indent
               (while (and (not (eobp)) (re-search-forward "^" nil t))
                 (replace-match indent t t)))
             (goto-char (point-min))
             (while (looking-at "[ \t]*\n") (replace-match ""))
             (goto-char (point-max))
             (when (> (org-current-line)
                      n-lines)
               (org-goto-line (1+ n-lines))
               (backward-char 1))
             (setq txt (buffer-substring (point-min) (point))))))))
    txt))

(defun org-roam-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error if there is no node at point.
This function also returns the node if it has yet to be cached in the
database. In this scenario, only expect `:id' and `:point' to be
populated."
  (if-let ((node (magit-section-case
                   (org-roam-node-section (oref it node))
                   (t (org-with-wide-buffer
                       (org-back-to-heading-or-point-min)
                       (while (and (not (org-roam-db-node-p))
                                   (not (bobp)))
                         (org-roam-up-heading-or-point-min))
                       (when-let ((id (org-id-get)))
                         (org-roam-populate
                          (org-roam-node-create
                           :id id
                           :point (point)))))))))
      node
    (when assert
      (user-error "No node at point"))))

(defun org-roam-node--find (node)
  "Navigate to the point for NODE, and return the buffer."
  (unless (org-roam-node-file node)
    (user-error "Node does not have corresponding file"))
  (let ((buf (find-file-noselect (org-roam-node-file node))))
    (with-current-buffer buf
      (goto-char (org-roam-node-point node)))
    buf))

(defun org-roam-node-visit (node &optional other-window)
  "From the current buffer, visit NODE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg))
  (let ((buf (org-roam-node--find node)))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defun org-roam-node-from-id (id)
  "Return an `org-roam-node' for the node containing ID.
Return nil if a node with ID does not exist."
  (when (> (caar (org-roam-db-query [:select (funcall count) :from nodes
                                     :where (= id $s1)]
                                    id)) 0)
    (org-roam-populate (org-roam-node-create :id id))))

(defun org-roam-node-from-title-or-alias (s)
  "Return an `org-roam-node' for the node with title or alias S.
Return nil if the node does not exist.
Throw an error if multiple choices exist."
  (let ((matches (seq-uniq
                  (append
                   (org-roam-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                      s)
                   (org-roam-db-query [:select [node-id] :from aliases
                                       :where (= alias $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(defun org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

(defun org-roam-node--to-candidate (node)
  "Return a minibuffer completion candidate given NODE."
  (let ((candidate-main (org-roam-node--format-entry node (1- (frame-width))))
        (tag-str (org-roam--tags-to-str (org-roam-node-tags node))))
    (cons (propertize (concat candidate-main
                              (propertize tag-str 'invisible t))
                      'node node)
          node)))

(defun org-roam-node--completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'.
The displayed title is formatted according to `org-roam-node-display-template'."
  (setq org-roam--cached-display-format nil)
  (let ((nodes (org-roam-node-list)))
    (mapcar #'org-roam-node--to-candidate nodes)))

(defcustom org-roam-node-annotation-function #'org-roam-node--annotation
  "The function used to return annotations in the minibuffer for Org-roam nodes.
This function takes a single argument NODE, which is an `org-roam-node' construct."
  :group 'org-roam
  :type 'function)

(defcustom org-roam-node-default-sort 'file-mtime
  "Default sort order for Org-roam node completions."
  :type 'const
  :group 'org-roam)

(defun org-roam-node-sort-by-file-mtime (completion-a completion-b)
  "Sort files such that files modified more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-node-struct)"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-mtime node-b)
                 (org-roam-node-file-mtime node-a))))

(defun org-roam-node-sort-by-file-atime (completion-a completion-b)
  "Sort files such that files accessed more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-node-struct)"
  "Sort completions list by file modification time."
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-atime node-b)
                 (org-roam-node-file-atime node-a))))

(defun org-roam-node-read (&optional initial-input filter-fn sort-fn require-match)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match."
  (let* ((nodes (org-roam-node--completions))
         (nodes (cl-remove-if-not (lambda (n)
                                    (if filter-fn (funcall filter-fn (cdr n)) t)) nodes))
         (sort-fn (or sort-fn
                      (when org-roam-node-default-sort
                        (intern (concat "org-roam-node-sort-by-" (symbol-name org-roam-node-default-sort))))))
         (_ (when sort-fn (setq nodes (seq-sort sort-fn nodes))))
         (node (completing-read
                "Node: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function . (lambda (title)
                                                 (funcall org-roam-node-annotation-function
                                                          (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (complete-with-action action nodes string pred)))
                nil require-match initial-input)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

(defun org-roam-node--annotation (_node)
  "Dummy function.
Returns empty string for annotations."
  "")

(defun org-roam-preview-visit (file point &optional other-window)
  "Visit FILE at POINT.
With prefix argument OTHER-WINDOW, visit the olp in another
window instead."
  (interactive (list (org-roam-file-at-point 'assert)
                     (oref (magit-current-section) point)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(cl-defun org-roam-node-insert-section (&key source-node point properties)
  "Insert section for a link from SOURCE-NODE to some other node.

SOURCE-NODE is an `org-roam-node' that links or references some
other node. Normally the other node is
`org-roam-buffer-current-node'.

POINT is the position in SOURCE-NODE's file where the link is
located.

PROPERTIES (a plist) contains additional information about the
link.

This section is made out of the next 2 `magit-section's:
1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE.

2. `org-roam-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat (propertize (org-roam-node-title source-node)
                                  'font-lock-face 'org-roam-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-get-preview (org-roam-node-file source-node) point))
              "\n")
      (oset section file (org-roam-node-file source-node))
      (oset section point point)
      (insert ?\n))))

;;;###autoload
(defun org-roam-node-find (&optional other-window initial-input filter-fn)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :props '(:finalize find-file)))))

;;;###autoload
(defun org-roam-node-insert (&optional filter-fn)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-title node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (org-roam-capture-
             :node node
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-node-random (&optional other-window)
  "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead."
  (interactive current-prefix-arg)
  (let ((random-row (seq-random-elt (org-roam-db-query [:select [id file pos] :from nodes]))))
    (org-roam-node-visit (org-roam-node-create :id (nth 0 random-row)
                                               :file (nth 1 random-row)
                                               :point (nth 2 random-row))
                         other-window)))

;;;; Properties
(defun org-roam-add-property (val prop)
  "Add VAL value to PROP property for the node at point.
Both, VAL and PROP are strings."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq val lst) lst (cons val lst)))
         (lst (seq-uniq lst)))
    (org-set-property prop (combine-and-quote-strings lst))
    val))

(defun org-roam-remove-property (prop &optional val)
  "Remove VAL value from PROP property for the node at point.
Both VAL and PROP are strings.

If VAL is not specified, user is prompted to select a value."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (prop-to-remove (or val (completing-read "Remove: " lst)))
         (lst (delete prop-to-remove lst)))
    (if lst
        (org-set-property prop (combine-and-quote-strings lst))
      (org-delete-property prop))
    prop-to-remove))

(defun org-roam-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" key ": " value "\n")))))

;;;; Tags
(defun org-roam-tag-completions ()
  "Return list of tags for completions within Org-roam."
  (let ((roam-tags (mapcar #'car (org-roam-db-query [:select :distinct [tag] :from tags])))
        (org-tags (cl-loop for tagg in org-tag-alist
                           nconc (pcase tagg
                                   ('(:newline)
                                    nil)
                                   (`(,tag . ,_)
                                    (list tag))
                                   (_ nil)))))
    (seq-uniq (append roam-tags org-tags))))

(defun org-roam-tag-add (tags)
  "Add TAGS to the node at point."
  (interactive
   (list (completing-read-multiple "Tag: " (org-roam-tag-completions))))
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                ""))))
            (org-roam-set-keyword "filetags" (string-join (seq-uniq (append tags current-tags)) " ")))
        (org-set-tags (seq-uniq (append tags (org-get-tags)))))
      tags)))

(defun org-roam-tag-remove (&optional tags)
  "Remove TAGS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let* ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                              (org-collect-keywords '("filetags"))))
                                                 (user-error "No tag to remove"))))
                 (tags (or tags (completing-read-multiple "Tag: " current-tags))))
            (org-roam-set-keyword "filetags"
                                  (string-join (seq-difference current-tags tags #'string-equal) " ")))
        (let* ((current-tags (or (org-get-tags)
                                 (user-error "No tag to remove")))
               (tags (completing-read-multiple "Tag: " current-tags)))
          (org-set-tags (seq-difference current-tags tags #'string-equal))))
      tags)))

;;;; Aliases
(defun org-roam-alias-add (alias)
  "Add ALIAS to the node at point."
  (interactive "sAlias: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-add-property alias "ROAM_ALIASES"))))

(defun org-roam-alias-remove (&optional alias)
  "Remove an ALIAS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-remove-property "ROAM_ALIASES" alias))))

;;;; Refs
(defun org-roam-ref-add (ref)
  "Add REF to the node at point."
  (interactive "sRef: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-add-property ref "ROAM_REFS"))))

(defun org-roam-ref-remove (&optional ref)
  "Remove a REF from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-remove-property "ROAM_REFS" ref))))

(defun org-roam-ref--completions ()
  "Return an alist for ref completion.
The car is the ref, and the cdr is the corresponding node for the ref."
  nil
  (let ((rows (org-roam-db-query
               [:select [id ref type nodes:file pos title]
                :from refs
                :left-join nodes
                :on (= refs:node-id nodes:id)])))
    (cl-loop for row in rows
             collect (pcase-let* ((`(,id ,ref ,type ,file ,pos ,title) row)
                                  (node (org-roam-node-create :id id
                                                              :file file
                                                              :point pos
                                                              :title title)))
                       (cons (propertize ref 'node node 'type type)
                             node)))))

(defun org-roam-ref-read (&optional initial-input filter-fn)
  "Read an Org-roam ref.
Return a string, is propertized in `meta' with additional properties.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
filtered out."
  (let* ((refs (org-roam-ref--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (completing-read "Ref: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     '(metadata
                                       (annotation-function . org-roam-ref--annotation)
                                       (category . org-roam-ref))
                                   (complete-with-action action refs string pred)))
                               nil t initial-input)))
    (cdr (assoc ref refs))))

(defun org-roam-ref--annotation (ref)
  "Return the annotation for REF.
REF is assumed to be a propertized string."
  (let* ((node (get-text-property 0 'node ref))
         (title (org-roam-node-title node)))
    (when title
      (concat " " title))))

;;;###autoload
(defun org-roam-ref-find (&optional initial-input filter-fn)
  "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (let* ((node (org-roam-ref-read initial-input filter-fn)))
    (find-file (org-roam-node-file node))
    (goto-char (org-roam-node-point node))))

;;;; The roam: link
(defcustom org-roam-link-auto-replace t
  "If non-nil, replace \"roam:\" links to existing nodes with \"id:\" links."
  :group 'org-roam
  :type 'boolean)

(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link)

(defun org-roam-link-replace-at-point (&optional link)
  "Replace \"roam:\" LINK at point with an \"id:\" link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   (setq node (org-roam-node-from-title-or-alias path)))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-node-id node))
                          path)))))))

(defun org-roam-link-replace-all ()
  "Replace all \"roam:\" links in buffer with \"id:\" links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (org-roam-link-replace-at-point))))

(defun org-roam--replace-roam-links-on-save-h ()
  "Run `org-roam-link-replace-all' before buffer is saved to its file."
  (when org-roam-link-auto-replace
    (add-hook 'before-save-hook #'org-roam-link-replace-all nil t)))

(add-hook 'org-roam-find-file-hook #'org-roam--replace-roam-links-on-save-h)

(defun org-roam-link-follow-link (title-or-alias)
  "Navigate \"roam:\" link to find and open the node with TITLE-OR-ALIAS.
Assumes that the cursor was put where the link is."
  (if-let ((node (org-roam-node-from-title-or-alias title-or-alias)))
      (progn
        (when org-roam-link-auto-replace
          (org-roam-link-replace-at-point))
        (org-id-goto (org-roam-node-id node)))
    (org-roam-capture-
     :node (org-roam-node-create :title title-or-alias)
     :props '(:finalize find-file))))

(defun org-roam-open-id-at-point ()
  "Try to navigate \"id:\" link to find and visit node with an assigned ID.
Assumes that the cursor was put where the link is."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      (let ((node (org-roam-populate (org-roam-node-create :id id))))
        (cond
         ((org-roam-node-file node)
          (org-mark-ring-push)
          (org-roam-node-visit node)
          t)
         (t nil))))))

(defun org-roam-open-id-with-org-roam-db-h ()
  "Try to open \"id:\" links at point by querying them to the database."
  (add-hook 'org-open-at-point-functions #'org-roam-open-id-at-point nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-open-id-with-org-roam-db-h)

;;;; Org-roam-refile
(defun org-roam-demote-entire-buffer ()
  "Convert an org buffer with any top level content to a single node.

All headings are demoted one level.

The #+TITLE: keyword is converted into a level-1 heading and deleted.
Any tags declared on #+FILETAGS: are transferred to tags on the new top heading.

Any top level properties drawers are incorporated into the new heading."
  (interactive)
  (org-with-point-at 1
    (org-map-entries 'org-do-demote)
    (insert "* "
            (org-roam--file-keyword-get "TITLE")
            "\n")
    (org-back-to-heading)
    (org-set-tags (org-roam--file-keyword-get "FILETAGS"))
    (org-roam--file-keyword-kill "TITLE")
    (org-roam--file-keyword-kill "FILETAGS")))

(defun org-roam-refile ()
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if regionp
        (progn
          (org-kill-new (buffer-substring region-start region-end))
          (org-save-markers-in-region region-start region-end))
      (progn
        (if (org-before-first-heading-p)
            (org-roam-demote-entire-buffer))
        (org-copy-subtree 1 nil t)))
    (with-current-buffer nbuf
      (org-with-wide-buffer
       (goto-char (org-roam-node-point node))
       (setq level (org-get-valid-level (funcall outline-level) 1)
             reversed (org-notes-order-reversed-p))
       (goto-char
        (if reversed
            (or (outline-next-heading) (point-max))
          (or (save-excursion (org-get-next-sibling))
              (org-end-of-subtree t t)
              (point-max))))
       (unless (bolp) (newline))
       (org-paste-subtree level nil nil t)
       (and org-auto-align-tags
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
       (when (fboundp 'deactivate-mark) (deactivate-mark))))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
    (org-roam--kill-empty-buffer)))

;;;; Completions
(defcustom org-roam-completion-everywhere nil
  "When non-nil, provide link completion matching outside of Org links."
  :group 'org-roam
  :type 'boolean)

(defvar org-roam-completion-functions (list #'org-roam-complete-link-at-point
                                            #'org-roam-complete-everywhere)
  "List of functions to be used with `completion-at-point' for Org-roam.")

(defconst org-roam-bracket-completion-re
  "\\[\\[\\(\\(?:roam:\\)?\\)\\([^z-a]*\\)]]"
  "Regex for completion within link brackets.
We use this as a substitute for `org-link-bracket-re', because
`org-link-bracket-re' requires content within the brackets for a match.")

(defun org-roam-complete-everywhere ()
  "Provides completions for links for any word at point.
This is a `completion-at-point' function, and is active when
`org-roam-completion-everywhere' is non-nil."
  (when (and org-roam-completion-everywhere
             (thing-at-point 'word)
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (lambda (_)
               (funcall #'org-roam--get-titles)))
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert "[[roam:" str "]]"))))))

(defun org-roam-complete-link-at-point ()
  "Do appropriate completion for the link at point."
  (let (roam-p start end)
    (when (org-in-regexp org-roam-bracket-completion-re 1)
      (setq roam-p (not (string-blank-p (match-string 1)))
            start (match-beginning 2)
            end (match-end 2))
      (list start end
            (completion-table-dynamic
             (lambda (_)
               (funcall #'org-roam--get-titles)))
            :exit-function
            (lambda (str &rest _)
              (delete-char (- 0 (length str)))
              (insert (concat (unless roam-p "roam:")
                              str))
              (forward-char 2))))))

(defun org-roam-complete-at-point ()
  "."
  (run-hook-with-args-until-success 'org-roam-completion-functions))

(defun org-roam--register-completion-functions ()
  "."
  (add-hook 'completion-at-point-functions #'org-roam-complete-at-point nil t))

(add-hook 'org-roam-find-file-hook #'org-roam--register-completion-functions)

;;;; The Org-roam Buffer
;;;;; Faces
(defface org-roam-header-line
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for the `header-line' in some Org-roam modes."
  :group 'org-roam-faces)

(defface org-roam-title
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

(defface org-roam-olp
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the OLP of the node."
  :group 'org-roam-faces)

(defface org-roam-preview-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey25"
     :foreground "grey70"))
  "Face for preview headings."
  :group 'org-roam-faces)

(defface org-roam-preview-heading-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey35"
     :foreground "grey70"))
  "Face for current preview headings."
  :group 'org-roam-faces)

(defface org-roam-preview-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit org-roam-preview-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit org-roam-preview-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected preview headings."
  :group 'org-roam-faces)

(defface org-roam-preview-region
  `((t :inherit bold
       ,@(and (>= emacs-major-version 27)
              (list :extend (ignore-errors (face-attribute 'region :extend))))))
  "Face used by `org-roam-highlight-preview-region-using-face'.

This face is overlaid over text that uses other hunk faces,
and those normally set the foreground and background colors.
The `:foreground' and especially the `:background' properties
should be avoided here.  Setting the latter would cause the
loss of information.  Good properties to set here are `:weight'
and `:slant'."
  :group 'org-roam-faces)

(defface org-roam-dim
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the dimmer part of the widgets."
  :group 'org-roam-faces)

;;;;; Variables
(defvar org-roam-buffer-current-node nil
  "The node for which an `org-roam-mode' based buffer displays its contents.
This set both, locally and globally. Normally the local value is
only set in the `org-roam-mode' based buffers, while the global
value shows the current node in the persistent `org-roam-buffer'.")

(put 'org-roam-buffer-current-node 'permanent-local t)

(defvar org-roam-buffer-current-directory nil
  "The `org-roam-directory' value of `org-roam-buffer-current-node'.
Set both, locally and globally in the same way as `org-roam-buffer-current-node'.")

(put 'org-roam-buffer-current-directory 'permanent-local t)

(defcustom org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                                 #'org-roam-reflinks-section)
  "Functions that insert sections in the `org-roam-mode' based buffers.
Each function is called with one argument, which is an
`org-roam-node' for which the buffer will be constructed for.
Normally this node is `org-roam-buffer-current-node'."
  :group 'org-roam
  :type 'hook)

(defun org-roam-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(defun org-roam-buffer-refresh ()
  "Refresh the contents of the currently selected Org-roam buffer."
  (interactive)
  (cl-assert (derived-mode-p 'org-roam-mode))
  (save-excursion (org-roam-buffer-render-contents)))

(defun org-roam-buffer-render-contents ()
  "Recompute and render the contents of an Org-roam buffer.
Assumes that the current buffer is an `org-roam-mode' based
buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-mode)
    (setq-local default-directory org-roam-buffer-current-directory)
    (setq-local org-roam-directory org-roam-buffer-current-directory)
    (org-roam-set-header-line-format
     (org-roam-node-title org-roam-buffer-current-node))
    (magit-insert-section (org-roam)
      (magit-insert-heading)
      (run-hook-with-args 'org-roam-mode-section-functions org-roam-buffer-current-node))
    (goto-char 0)))

;;;;; Dedicated buffer
;;;###autoload
(defun org-roam-buffer-display-dedicated (node)
  "Launch NODE dedicated Org-roam buffer.
Unlike the persistent `org-roam-buffer', the contents of this
buffer won't be automatically changed and will be held in place.

In interactive calls prompt to select NODE, unless called with
`universal-argument', in which case NODE will be set to
`org-roam-node-at-point'."
  (interactive
   (list (if current-prefix-arg
             (org-roam-node-at-point 'assert)
           (org-roam-node-read nil nil nil 'require-match))))
  (let ((buffer (get-buffer-create (org-roam-buffer--dedicated-name node))))
    (with-current-buffer buffer
      (setq-local org-roam-buffer-current-node node)
      (setq-local org-roam-buffer-current-directory org-roam-directory)
      (org-roam-buffer-render-contents))
    (display-buffer buffer)))

(defun org-roam-buffer--dedicated-name (node)
  "Construct buffer name for NODE dedicated Org-roam buffer."
  (let ((title (org-roam-node-title node))
        (filename (file-relative-name (org-roam-node-file node) org-roam-directory)))
    (format "*org-roam: %s<%s>*" title filename)))

(defun org-roam-buffer-dedicated-p (&optional buffer)
  "Return t if an Org-roam BUFFER is a node dedicated one.
See `org-roam-buffer-display-dedicated' for more details.
If BUFFER is nil, default it to `current-buffer'."
  (or buffer (setq buffer (current-buffer)))
  (string-match-p (concat "^" (regexp-quote "*org-roam: "))
                  (buffer-name buffer)))

;;;;; Persistent buffer
(defvar org-roam-buffer "*org-roam*"
  "The persistent Org-roam buffer name. Must be surround with \"*\".
The content inside of this buffer will be automatically updated
to the nearest node at point that comes from the current buffer.
To toggle its display use `org-roam-buffer-toggle' command.")

(defun org-roam-buffer-toggle ()
  "Toggle display of the persistent `org-roam-buffer'."
  (interactive)
  (pcase (org-roam-buffer--visibility)
    ('visible
     (progn
       (delete-window (get-buffer-window org-roam-buffer))
       (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h)))
    ((or 'exists 'none)
     (progn
       (display-buffer (get-buffer-create org-roam-buffer))
       (org-roam-buffer-persistent-redisplay)))))

(define-inline org-roam-buffer--visibility ()
  "Return the current visibility state of the persistent `org-roam-buffer'.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam-buffer--persistent-cleanup-h ()
  "Clean-up global state thats dedicated for the persistent `org-roam-buffer'."
  (setq-default org-roam-buffer-current-node nil
                org-roam-buffer-current-directory nil))

(defun org-roam-buffer-persistent-redisplay ()
  "Recompute contents of the persistent `org-roam-buffer'.
Has no effect when there's no `org-roam-node-at-point'."
  (when-let ((node (org-roam-node-at-point)))
    (unless (equal node org-roam-buffer-current-node)
      (setq org-roam-buffer-current-node node
            org-roam-buffer-current-directory org-roam-directory)
      (with-current-buffer (get-buffer-create org-roam-buffer)
        (org-roam-buffer-render-contents)
        (add-hook 'kill-buffer-hook #'org-roam-buffer--persistent-cleanup-h nil t)))))

(defun org-roam-buffer--redisplay-h ()
  "Reconstruct the persistent `org-roam-buffer'.
This needs to be quick or infrequent, because this designed to
run at `post-command-hook'."
  (and (get-buffer-window org-roam-buffer)
       (org-roam-buffer-persistent-redisplay)))

(defun org-roam-buffer--setup-redisplay-h ()
  "Setup automatic redisplay of the persistent `org-roam-buffer'."
  (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-buffer--setup-redisplay-h)

;;;;; Sections
;;;;;; Backlinks
(cl-defstruct (org-roam-backlink (:constructor org-roam-backlink-create)
                                 (:copier nil))
  source-node target-node
  point properties)

(cl-defmethod org-roam-populate ((backlink org-roam-backlink))
  "Populate BACKLINK from database."
  (setf (org-roam-backlink-source-node backlink)
        (org-roam-populate (org-roam-backlink-source-node backlink))
        (org-roam-backlink-target-node backlink)
        (org-roam-populate (org-roam-backlink-target-node backlink)))
  backlink)

(defun org-roam-backlinks-get (node)
  "Return the backlinks for NODE."
  (let ((backlinks (org-roam-db-query
                    [:select [source dest pos properties]
                     :from links
                     :where (= dest $s1)
                     :and (= type "id")]
                    (org-roam-node-id node))))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       (org-roam-populate
                        (org-roam-backlink-create
                         :source-node (org-roam-node-create :id source-id)
                         :target-node (org-roam-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

(defun org-roam-backlinks-sort (a b)
  "Default sorting function for backlinks A and B.
Sorts by title."
  (string< (org-roam-node-title (org-roam-backlink-source-node a))
           (org-roam-node-title (org-roam-backlink-source-node b))))

(defun org-roam-backlinks-section (node)
  "The backlinks section for NODE."
  (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort (org-roam-backlinks-get node))))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (org-roam-node-insert-section
         :source-node (org-roam-backlink-source-node backlink)
         :point (org-roam-backlink-point backlink)
         :properties (org-roam-backlink-properties backlink)))
      (insert ?\n))))

;;;;;; Reflinks
(cl-defstruct (org-roam-reflink (:constructor org-roam-reflink-create)
                                (:copier nil))
  source-node ref
  point properties)

(cl-defmethod org-roam-populate ((reflink org-roam-reflink))
  "Populate REFLINK from database."
  (setf (org-roam-reflink-source-node reflink)
        (org-roam-populate (org-roam-reflink-source-node reflink)))
  reflink)

(defun org-roam-reflinks-get (node)
  "Return the reflinks for NODE."
  (let ((refs (org-roam-db-query [:select [ref] :from refs
                                  :where (= node-id $s1)]
                                 (org-roam-node-id node)))
        links)
    (pcase-dolist (`(,ref) refs)
      (pcase-dolist (`(,source-id ,pos ,properties) (org-roam-db-query
                                                     [:select [source pos properties]
                                                      :from links
                                                      :where (= dest $s1)]
                                                     ref))
        (push (org-roam-populate
               (org-roam-reflink-create
                :source-node (org-roam-node-create :id source-id)
                :ref ref
                :point pos
                :properties properties)) links)))
    links))

(defun org-roam-reflinks-sort (a b)
  "Default sorting function for reflinks A and B.
Sorts by title."
  (string< (org-roam-node-title (org-roam-reflink-source-node a))
           (org-roam-node-title (org-roam-reflink-source-node b))))

(defun org-roam-reflinks-section (node)
  "The reflinks section for NODE."
  (when (org-roam-node-refs node)
    (let* ((reflinks (seq-sort #'org-roam-reflinks-sort (org-roam-reflinks-get node))))
      (magit-insert-section (org-roam-reflinks)
        (magit-insert-heading "Reflinks:")
        (dolist (reflink reflinks)
          (org-roam-node-insert-section
           :source-node (org-roam-reflink-source-node reflink)
           :point (org-roam-reflink-point reflink)
           :properties (org-roam-reflink-properties reflink)))
        (insert ?\n)))))

;;;;;; Unlinked references
(defvar org-roam-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-file-visit)
    map)
  "Keymap for Org-roam grep result sections.")

(defclass org-roam-grep-section (magit-section)
  ((keymap :initform 'org-roam-grep-map)
   (file :initform nil)
   (row :initform nil)
   (col :initform nil)))

(defun org-roam-file-at-point (&optional assert)
  "Return the file at point.
If ASSERT, throw an error."
  (if-let ((file (magit-section-case
                   (org-roam-node-section (org-roam-node-file (oref it node)))
                   (org-roam-grep-section (oref it file))
                   (org-roam-preview-section (oref it file)))))
      file
    (when assert
      (user-error "No file at point"))))

(defun org-roam-file-visit (file &optional other-window row col)
  "Visits FILE.
With a prefix argument OTHER-WINDOW, display the buffer in
another window instead.
If ROW, move to the row, and if COL move to the COL."
  (interactive (list (org-roam-file-at-point t)
                     current-prefix-arg
                     (oref (magit-current-section) row)
                     (oref (magit-current-section) col)))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (when row
        (forward-line (1- row)))
      (when col
        (forward-char (1- col))))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defvar org-roam-unlinked-references-result-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

(defun org-roam-unlinked-references-preview-line (file row)
  "Return the preview line from FILE.
This is the ROW within FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (forward-line (1- row))
    (buffer-substring-no-properties
     (save-excursion
       (beginning-of-line)
       (point))
     (save-excursion
       (end-of-line)
       (point)))))

(defun org-roam-unlinked-references-section (node)
  "The unlinked references section for NODE.
References from FILE are excluded."
  (when (and (executable-find "rg")
             (not (string-match "PCRE2 is not available"
                                (shell-command-to-string "rg --pcre2-version"))))
    (let* ((titles (cons (org-roam-node-title node)
                         (org-roam-node-aliases node)))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (mapconcat (lambda (glob) (concat "-g " glob))
                                          (org-roam--list-files-search-globs org-roam-file-extensions)
                                          " ")
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-roam-directory))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           f row col match)
      (magit-insert-section (unlinked-references)
        (magit-insert-heading "Unlinked References:")
        (dolist (line results)
          (save-match-data
            (when (string-match org-roam-unlinked-references-result-re line)
              (setq f (match-string 1 line)
                    row (string-to-number (match-string 2 line))
                    col (string-to-number (match-string 3 line))
                    match (match-string 4 line))
              (when (and match
                         (not (f-equal-p (org-roam-node-file node) f))
                         (member (downcase match) (mapcar #'downcase titles)))
                (magit-insert-section section (org-roam-grep-section)
                  (oset section file f)
                  (oset section row row)
                  (oset section col col)
                  (insert (propertize (format "%s:%s:%s"
                                              (truncate-string-to-width (file-name-base f) 15 nil nil "...")
                                              row col) 'font-lock-face 'org-roam-dim)
                          " "
                          (org-roam-fontify-like-in-org-mode
                           (org-roam-unlinked-references-preview-line f row))
                          "\n"))))))
        (insert ?\n)))))

(provide 'org-roam)
;;; org-roam.el ends here
;; Local Variables:
;; eval: (require 'outshine nil t)
;; eval: (when (boundp 'outshine-mode) (outshine-mode +1))
;; End:
