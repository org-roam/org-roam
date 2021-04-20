;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (magit-section "2.90.1"))

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
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)
(require 'seq)
(require 'magit-section)
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-compat)
(eval-when-compile
  (require 'org-roam-macs))
(require 'org-roam-utils)
(require 'org-roam-mode)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-dailies)
(require 'org-roam-db)

;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

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

;;;; Variables
(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

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
    '(find rg))
  "Commands that will be used to find Org-roam files.

It should be a list of symbols or cons cells representing any of the following
 supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

  `find'
    Use find as the file search method.
    Example command:
    find /path/to/dir -type f \( -name \"*.org\" -o -name \"*.org.gpg\" \)

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "find" find)
              (const :tag "rg" rg)))

(defcustom org-roam-slug-trim-chars
  '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
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
    )
  "Characters to trim from Unicode normalization for slug.

By default, the characters are specified to remove Diacritical Marks from the Latin alphabet."
  :type '(repeat character)
  :group 'org-roam)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

;;;; ID Utilities
(defun org-roam-id-at-point ()
  "Return the ID at point, if any.
Recursively traverses up the headline tree to find the
first encapsulating ID."
  (let (source)
    (org-with-wide-buffer
     (while (and (not (setq source (org-id-get)))
                 (not (bobp)))
       (org-roam-up-heading-or-point-min)))
    source))

;;;; File functions and predicates
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
    (when (string= ext "gpg")           ; Handle encrypted files
      (setq ext (org-roam--file-name-extension (file-name-sans-extension path))))
    (member ext org-roam-file-extensions)))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (or file
                       (buffer-file-name (buffer-base-buffer)))))
    (save-match-data
      (and
       (org-roam--org-file-p path)
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

;; Emacs 26 does not have FOLLOW-SYMLINKS in `directory-files-recursively'
(defun org-roam--directory-files-recursively (dir regexp
                                                  &optional include-directories predicate
                                                  follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (condition-case nil
                               (org-roam--directory-files-recursively
                                full-file regexp include-directories
                                predicate follow-symlinks)
                             (file-error nil))
                         (org-roam--directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let* ((file-regex (concat "\\.\\(?:"
                             (mapconcat #'regexp-quote org-roam-file-extensions "\\|")
                             "\\)\\(?:\\.gpg\\)?\\'"))
         (files (org-roam--directory-files-recursively dir file-regex nil nil t))
         result)
    (dolist (file files result)
      (when (and (file-readable-p file)
                 (org-roam--org-file-p file))
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
              (files (seq-filter #'org-roam--org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

(defun org-roam--tags-table ()
  "Return a hash table of node ID to list of tags."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,node-id ,tag) (org-roam-db-query [:select [node-id tag] :from tags]))
      (puthash node-id (cons tag (gethash node-id ht)) ht))
    ht))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam--get-titles ()
  "Return all titles and aliases in the Org-roam database."
  (let* ((titles (mapcar #'car (org-roam-db-query [:select title :from nodes])))
         (aliases (mapcar #'car (org-roam-db-query [:select alias :from aliases])))
         (completions (append titles aliases)))
    completions))

;;; Org-roam setup and teardown
(defvar org-roam-find-file-hook nil
  "Hook run when an Org-roam file is visited.")

(defun org-roam-setup ()
  "Setup Org-roam."
  (interactive)
  (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                   (file-executable-p emacsql-sqlite3-executable))
              (executable-find "sqlite3"))
    (lwarn '(org-roam) :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at Org-roam > Installation > Post-Installation Tasks."))
  (add-hook 'find-file-hook #'org-roam--file-setup)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-add 'rename-file :after #'org-roam--rename-file-advice)
  (advice-add 'delete-file :before #'org-roam--delete-file-advice)
  (org-roam-db-sync))

(defun org-roam-teardown ()
  "Teardown Org-roam."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam--file-setup)
  (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-remove 'rename-file #'org-roam--rename-file-advice)
  (advice-remove 'delete-file #'org-roam--delete-file-advice)
  (org-roam-db--close-all)
  ;; Disable local hooks for all org-roam buffers
  (dolist (buf (org-roam--get-roam-buffers))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-db-update-file t))))

;;; Hooks and advices
(defun org-roam--file-setup ()
  "Setup an Org-roam file."
  (when (org-roam--org-roam-file-p)
    (run-hooks 'org-roam-find-file-hook)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam--org-roam-file-p file))
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
               (org-roam--org-roam-file-p old-file))
      (org-roam-db-clear-file old-file))
    (when (org-roam--org-roam-file-p new-file)
      (org-roam-db-update-file new-file))))

;;;; Nodes
(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
                             (:copier nil))
  id file level point todo priority scheduled deadline title
  tags aliases refs)

(cl-defmethod org-roam-node-slug ((node org-roam-node))
  "Return the slug of NODE."
  (let ((title (org-roam-node-title node)))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char org-roam-slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                      ("__*" . "_")  ;; remove sequential underscores
                      ("^_" . "")  ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-node-visit)
    map)
  "Keymap for Org-roam node sections.")

(defclass org-roam-node-section (magit-section)
  ((keymap :initform org-roam-node-map)
   (node :initform nil)))

(defvar org-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-preview-visit)
    map)
  "Keymap for Org-roam preview.")

(defclass org-roam-preview-section (magit-section)
  ((keymap :initform org-roam-preview-map)
   (file :initform nil)
   (begin :initform nil)
   (end :initform nil)))

(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (let ((node-info (car (org-roam-db-query [:select [file level pos todo priority scheduled deadline title]
                                            :from nodes
                                            :where (= id $s1)
                                            :limit 1]
                                           (org-roam-node-id node))))
        (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                    :where (= node-id $s1)]
                                                   (org-roam-node-id node))))
        (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                      :where (= node-id $s1)]
                                                     (org-roam-node-id node))))
        (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                     :where (= node-id $s1)]
                                                    (org-roam-node-id node)))))
    (pcase-let ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title) node-info))
      (setf (org-roam-node-file node) file
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info))
    node))

(defcustom org-roam-node-display-template
  "${title:48}   ${tags:10}"
  "Configures display formatting for Org-roam node."
  :group 'org-roam
  :type  'string)

(defun org-roam--tags-to-str (tags)
  "Convert list of TAGS into a string."
  (string-join
   (mapcar (lambda (s) (concat "#" s)) tags)
   " "))

(defun org-roam-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list."
  (let ((format (org-roam--process-display-format org-roam-node-display-template)))
    (s-format (car format)
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
                  (if (not field-width)
                      field-value
                    (setq field-width (string-to-number field-width))
                    (truncate-string-to-width
                     field-value
                     (if (> field-width 0)
                         field-width
                       (- width (cdr format)))
                     0 ?\s)))))))

(defun org-roam-node-preview (file point)
  "Get preview content for FILE at POINT."
  (save-excursion
    (org-roam-with-temp-buffer file
      (goto-char point)
      (let* ((elem (org-element-at-point))
             (begin (org-element-property :begin elem))
             (end (org-element-property :end elem)))
        (list begin end
              (or (string-trim (buffer-substring-no-properties begin end))
                  (org-element-property :raw-value elem)))))))

(defun org-roam-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error."
  (if-let ((node (magit-section-case
                   (org-roam-node-section (oref it node))
                   (t (when-let ((id (org-roam-id-at-point)))
                        (org-roam-populate (org-roam-node-create :id id)))))))
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
  "From the buffer, visit NODE.

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
                                       :left :join nodes :on (= nodes:id aliases:node-id)
                                       :where (= aliases:node-id $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(defun org-roam-node--completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'."
  (let ((tags-table (org-roam--tags-table)))
    (cl-loop for row in (append
                         (org-roam-db-query [:select [file pos title title id]
                                             :from nodes])
                         (org-roam-db-query [:select [nodes:file pos alias title node-id]
                                             :from aliases
                                             :left-join nodes
                                             :on (= aliases:node-id nodes:id)]))
             collect (pcase-let* ((`(,file ,pos ,alias ,title ,id) row)
                                  (node (org-roam-node-create :id id
                                                              :file file
                                                              :title title
                                                              :point pos
                                                              :tags (gethash id tags-table)))
                                  (candidate-main (org-roam-node--format-entry node (1- (frame-width))))
                                  (tag-str (org-roam--tags-to-str (org-roam-node-tags node))))
                       (cons (propertize (concat (propertize tag-str 'invisible t)
                                                 candidate-main
                                                 (propertize alias 'invisible t))
                                         'node node)
                             node)))))

(defcustom org-roam-node-annotation-function #'org-roam-node--annotation
  "The function used to return annotations in the minibuffer for Org-roam nodes.
This function takes a single argument NODE, which is an `org-roam-node' construct.")

(defun org-roam-node-read (&optional initial-input filter-fn require-match)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function applied to the completion list.
If REQUIRE-MATCH, require returning a match."
  (let* ((nodes (org-roam-node--completions))
         (nodes (funcall (or filter-fn #'identity) nodes))
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
  (interactive (list (org-roam-file-at-point t)
                     (oref (magit-current-section) begin)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(cl-defun org-roam-node-insert-section (&key source-node point properties)
  "Insert section for NODE.
SOURCE-NODE is the source node.
POINT is the point in buffer for the link.
PROPERTIES contains properties about the link."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (string-join (mapcar #'org-link-display-format outline)
                                    " > ")
                     "Top")))
      (insert (concat (propertize (org-roam-node-title source-node)
                                  'font-lock-face 'org-roam-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-preview-section)
      (pcase-let ((`(,begin ,end ,s) (org-roam-node-preview (org-roam-node-file source-node)
                                                            point)))
        (insert (org-roam-fontify-like-in-org-mode s) "\n")
        (oset section file (org-roam-node-file source-node))
        (oset section begin begin)
        (oset section end end)))))

;;;###autoload
(defun org-roam-node-find (&optional other-window initial-input filter-fn)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :props '(:finalize find-file)))))

(defun org-roam-node-insert (&optional filter-fn)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
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
             :props (list :region (when (and beg end)
                                    (cons beg end))
                          :insert-at (point-marker)
                          :link-description description
                          :finalize 'insert-link)))))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-node-random (&optional other-window)
  "Find a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead."
  (interactive current-prefix-arg)
  (let ((random-row (seq-random-elt (org-roam-db-query [:select [id file pos] :from nodes]))))
    (org-roam-node-visit (org-roam-node-create :id (nth 0 random-row)
                                               :file (nth 1 random-row)
                                               :point (nth 2 random-row))
                         other-window)))

;;;; Properties
(defun org-roam-add-property (s prop)
  "Add S to property PROP."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq s lst) lst (cons s lst))))
    (org-set-property prop (combine-and-quote-strings lst))))

(defun org-roam-remove-property (prop)
  "Prompt to remove an item from PROP."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (prop-to-remove (completing-read "Remove: " lst))
         (lst (delete prop-to-remove lst)))
    (if lst
        (org-set-property prop (combine-and-quote-strings lst))
      (org-delete-property prop))))

;;;; Aliases
(defun org-roam-alias-add (alias)
  "Add ALIAS to the node at point."
  (interactive "sAlias: ")
  (org-roam-add-property alias "ROAM_ALIASES"))

(defun org-roam-alias-remove ()
  "Remove an alias from the node at point."
  (interactive)
  (org-roam-remove-property "ROAM_ALIASES"))

;;;; Refs
(defun org-roam-ref-add (ref)
  "Add REF to the node at point."
  (interactive "sRef: ")
  (org-roam-add-property ref "ROAM_REFS"))

(defun org-roam-ref-remove ()
  "Remove a ref from the node at point."
  (interactive)
  (org-roam-remove-property "ROAM_REFS"))

;;;; Backlinks
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

(defun org-roam-backlinks-insert-section (node)
  "Insert backlinks section for NODE."
  (let* ((backlinks (seq-sort #'org-roam-backlinks-sort (org-roam-backlinks-get node))))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (org-roam-node-insert-section
         :source-node (org-roam-backlink-source-node backlink)
         :point (org-roam-backlink-point backlink)
         :properties (org-roam-backlink-properties backlink)))
      (insert ?\n))))

;;;; Refs
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
FILTER-FN is a function applied to the completion list."
  (let* ((refs (org-roam-ref--completions))
         (refs (funcall (or filter-fn #'identity) refs))
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

(defun org-roam-ref-find (&optional initial-input filter-fn)
  "Find and open and Org-roam file from REF if it exists.
REF should be the value of '#+roam_key:' without any
type-information (e.g. 'cite:').
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is applied to the ref list to filter out candidates."
  (interactive)
  (let* ((node (org-roam-ref-read initial-input filter-fn)))
    (find-file (org-roam-node-file node))
    (goto-char (org-roam-node-point node))))

;;;; Reflinks
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

(defun org-roam-reflinks-insert-section (node)
  "Insert reflinks section for NODE."
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

;;;; Unlinked references
(defvar org-roam-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-file-visit)
    map)
  "Keymap for Org-roam grep result sections.")

(defclass org-roam-grep-section (magit-section)
  ((keymap :initform org-roam-grep-map)
   (file :initform nil)
   (row :initform nil)
   (col :initform nil)))

(defun org-roam-file-at-point (&optional assert)
  "Return the file at point.
If ASSERT, throw an error."
  (if-let ((file (magit-section-case
                   (org-roam-node-section (org-roam-node-file (oref it node)))
                   (org-roam-grep-section (oref it file))
                   (org-roam-olp-section (oref it file))
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
    (insert-file-contents-literally file)
    (forward-line (1- row))
    (buffer-substring-no-properties
     (save-excursion
       (beginning-of-line)
       (point))
     (save-excursion
       (end-of-line)
       (point)))))

(defun org-roam-unlinked-references-insert-section (node)
  "Render unlinked references for NODE.
References from FILE are excluded."
  (when (and (executable-find "rg")
             (not (string-match "PCRE2 is not available"
                                (shell-command-to-string "rg --pcre2-version"))))
    (let* ((titles (cons (org-roam-node-title node)
                         (org-roam-node-aliases node)))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                    (org-roam--list-files-search-globs
                                                     org-roam-file-extensions)) " ")
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

;;;; roam: link
(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file or id links whenever possible."
  :group 'org-roam
  :type 'boolean)

;;; the roam: link
(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link)

(defun org-roam-link-follow-link (path)
  "Navigates to roam: link with description PATH.
This function is called by Org when following links of the type
`roam'. While the path is passed, assume that the cursor is on
the link."
  (pcase (org-element-lineage (org-element-context) '(link) t)
    ('nil (error "Not at Org link"))
    (link
     (if (not (string-equal "roam" (org-element-property :type link)))
         (error "Not at an Org-roam link")
       (let* ((title (org-element-property :path link))
              (node (org-roam-node-from-title-or-alias title)))
         (when org-roam-link-auto-replace
           (save-excursion
             (save-match-data
               (org-in-regexp org-link-bracket-re 1)
               (replace-match (org-link-make-string
                               (concat "id:" (org-roam-node-id node))
                               title)))))
         (org-id-goto (org-roam-node-id node)))))))

(provide 'org-roam)
;;; org-roam.el ends here
