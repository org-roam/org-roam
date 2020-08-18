;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

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
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-compat)
(require 'org-roam-macs)
;; These features should be able to be loaded order independently.
;; @TODO: implement something akin to `org-modules' that allows
;; selectively loading different sets of features.
;; ~NV [2020-05-22 Fri]

(require 'org-roam-faces)
(require 'org-roam-buffer)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-dailies)
(require 'org-roam-db)
(require 'org-roam-doctor)
(require 'org-roam-graph)

;;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(defvar org-id-link-to-org-use-id)
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

;;;; Customizable variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual/"))

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .gpg extension."
  :type 'boolean
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

(defcustom org-roam-find-file-function nil
  "Function called when visiting files in Org-roam commands.
If nil, `find-file' is used."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-include-type-in-ref-path-completions nil
  "When t, include the type in ref-path completions.
Note that this only affects interactive calls.
See `org-roam--get-ref-path-completions' for details."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-index-file "index.org"
  "Path to the Org-roam index file.
The path can be a string or a function.  If it is a string, it
should be the path (absolute or relative to `org-roam-directory')
to the index file.  If it is is a function, the function should
return the path to the index file.  Otherwise, the index is
assumed to be a note in `org-roam-directory' whose title is
'Index'."
  :type '(choice
          (string :tag "Path to index" "%s")
          (function :tag "Function to generate the path"))
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The formatter used when inserting Org-roam links that use their title.
Formatter may be a function that takes title as its only argument."
  :type '(choice
          (string :tag "String Format" "%s")
          (function :tag "Custom function"))
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

(defcustom org-roam-tag-separator ","
  "String to use to separate tags when `org-roam-tag-sources' is non-nil."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-tag-sort nil
  "When non-nil, sort the tags in the completions.
When t, sort the tags alphabetically, regardless of case.
`org-roam-tag-sort' can also be a list of arguments to be applied
to `cl-sort'.  For example, these are the arguments used when
`org-roam-tag-sort' is set to t:
    \('string-lessp :key 'downcase)
Only relevant when `org-roam-tag-sources' is non-nil."
  :type '(choice
          (boolean)
          (list :tag "Arguments to cl-loop"))
  :group 'org-roam)

(defcustom org-roam-tag-sources '(prop)
  "Sources to obtain tags from.

It should be a list of symbols representing any of the following
extraction methods:

  `prop'
    Extract tags from the #+roam_tags property.
    Tags are space delimited.
    Tags may contain spaces if they are double-quoted.
    e.g. #+roam_tags: TAG \"tag with spaces\"

  `all-directories'
    Extract sub-directories relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tags \"foo\" and \"bar\".

  `last-directory'
    Extract the last directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"bar\".

  `first-directory'
    Extract the first directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"foo\"."
  :type '(set (const :tag "#+roam_tags" prop)
              (const :tag "sub-directories" all-directories)
              (const :tag "parent directory" last-directory)
              (const :tag "first sub-directory" first-directory)))

(defcustom org-roam-title-to-slug-function #'org-roam--title-to-slug
  "Function to be used in converting a title to the filename slug.
Function should return a filename string based on title."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-title-sources '((title headline) alias)
  "The list of sources from which to retrieve a note title.
Each element in the list is either:

1. a symbol -- this symbol corresponds to a title retrieval
function, which returns the list of titles for the current buffer
2. a list of symbols -- symbols in the list are treated as
with (1).  The return value of this list is the first symbol in
the list returning a non-nil value.

The return results of the root list are concatenated.

For example the setting: '((title headline) alias) means the following:

1. Return the 'title + 'alias, if the title of current buffer is non-empty;
2. Or return 'headline + 'alias otherwise.

The currently supported symbols are:
1. 'title: The \"#+title\" property of org file.
2. 'alias: The \"#+roam_alias\" property of the org file, using
space-delimited strings.
3. 'headline: The first headline in the org file."
  :type '(repeat
          (choice
           (repeat symbol)
           (symbol)))
  :group 'org-roam)

(defcustom org-roam-enable-headline-linking t
  "Enable linking to headlines, which includes automatic :ID: creation and scanning of :ID:s for org-roam database."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

;;;; Dynamic variables
(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defvar-local org-roam-file-name nil
  "The corresponding file for a temp buffer.
This is set by `org-roam--with-temp-buffer', to allow throwing of
descriptive warnings when certain operations fail (e.g. parsing).")

(defvar org-roam--org-link-bracket-typed-re
  (rx (seq "[["
           (group (+? anything))
           ":"
           (group
            (one-or-more
             (or (not (any "[]\\"))
                 (and "\\" (zero-or-more "\\\\") (any "[]"))
                 (and (one-or-more "\\") (not (any "[]"))))))
           "]"
           (opt "[" (group (+? anything)) "]")
           "]"))
  "Matches a typed link in double brackets.")

(defvar org-roam--file-update-timer nil
  "Timer for updating the database on file changes.")

(defvar org-roam--file-update-queue nil
  "List of files that need to be processed for a database update. Processed within `org-roam--file-update-timer'.")

;;;; Utilities
(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam--str-to-list (str)
  "Transform string STR into a list of strings.
If STR is nil, return nil.

This function can throw an error if STR is not a string, or if
str is malformed (e.g. missing a closing quote). Callers of this
function are expected to catch the error."
  (when str
    (unless (stringp str)
      (signal 'wrong-type-argument `(stringp ,str)))
    (let* ((str (org-trim str))
           (format-str ":dummy '(%s)") ;The :dummy key is discarded in the `lst' var below.
           (items (cdar (org-babel-parse-header-arguments (format format-str str)))))
      (mapcar (lambda (item)
                (cond
                 ((stringp item)
                  item)
                 ((symbolp item)
                  (symbol-name item))
                 ((numberp item)
                  (number-to-string item))
                 (t
                  (signal 'wrong-type-argument `((stringp numberp symbolp) ,item))))) items))))

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
  (if-let ((path (or file
                     (buffer-file-name))))
      (save-match-data
        (and
         (org-roam--org-file-p path)
         (not (and org-roam-file-exclude-regexp
                   (string-match-p org-roam-file-exclude-regexp path)))
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory))))))

(defun org-roam--org-roam-headline-p (&optional id)
  "Return t if ID is part of Org-roam system, nil otherwise.
If ID is not specified, use the ID of the entry at point."
  (if-let ((id (or id
                   (org-id-get))))
      (org-roam-db-query [:select [file] :from headlines
                          :where (= id $s1)]
                         id)))

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
  (append
   (mapcar (lambda (ext) (s-wrap (concat "*." ext) "\"")) exts)
   (mapcar (lambda (ext) (s-wrap (concat "*." ext ".gpg") "\"")) exts)))

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\("
                                            ,(s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)) "\\)"))))
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
  (let ((regex (concat "\\.\\(?:"(mapconcat #'regexp-quote org-roam-file-extensions "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (org-roam--directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file) (org-roam--org-file-p file))
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
                   (when (string-equal (org-element-property :key kw) prop)
                     (org-element-property :value kw)))
                 :first-match t)))
        (push (cons prop p) res)))
    res))

(defun org-roam--expand-links (content path)
  "Crawl CONTENT for relative links and expand them.
PATH should be the root from which to compute the relativity."
  (let ((dir (file-name-directory path))
        link link-type)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Loop over links
      (while (re-search-forward org-roam--org-link-bracket-typed-re (point-max) t)
        (goto-char (match-beginning 2))
        (setq link-type (match-string 1)
              link (match-string 2))
        ;; Delete relative link
        (when (and (member link-type '("file")) ; TODO: Fix this
                   (f-relative-p link))
          (delete-region (match-beginning 2)
                         (match-end 2))
          (insert (expand-file-name link dir))))
      (buffer-string))))

(defun org-roam--get-outline-path ()
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as a
list of strings. Statistics cookies are removed and links are
kept.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline."
  (org-with-wide-buffer
   (save-match-data
     (and (or (condition-case nil
                  (org-back-to-heading t)
                (error nil))
              (org-up-heading-safe))
          (reverse (org-roam--get-outline-path-1))))))

(defun org-roam--get-outline-path-1 ()
  "Return outline path to current headline.

Outline path is a list of strings, in reverse order.  See
`org-roam--get-outline-path' for details.

Assume buffer is widened and point is on a headline."
  (when org-complex-heading-regexp
    (let ((heading (let ((case-fold-search nil))
                     (looking-at org-complex-heading-regexp)
                     (if (not (match-end 4)) ""
                       ;; Remove statistics cookies.
                       (org-trim
                        (replace-regexp-in-string
                         "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                         (match-string-no-properties 4)))))))
      (if (org-up-heading-safe)
          (cons heading (org-roam--get-outline-path-1))
        (list heading)))))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [from to type properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (require 'org-ref nil t)
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name))))
        links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let* ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (start (org-element-property :begin link)))
          (goto-char start)
          (let* ((element (org-element-at-point))
                 (begin (or (org-element-property :content-begin element)
                            (org-element-property :begin element)))
                 (content (or (org-element-property :raw-value element)
                              (buffer-substring-no-properties
                               begin
                               (or (org-element-property :content-end element)
                                   (org-element-property :end element)))))
                 (content (string-trim content))
                 ;; Expand all relative links to absolute links
                 (content (org-roam--expand-links content file-path)))
            (let ((properties (list :outline (mapcar (lambda (path)
                                                       (org-roam--expand-links path file-path))
                                                     (org-roam--get-outline-path))
                                    :content content
                                    :point begin))
                  (names (pcase type
                           ("id"
                            (list (car (org-roam-id-find path))))
                           ((pred (lambda (typ)
                                    (and (boundp 'org-ref-cite-types)
                                         (-contains? org-ref-cite-types typ))))
                            (setq type "cite")
                            (org-ref-split-and-strip-string path))
                           ("fuzzy" (list path))
                           (_ (if (file-remote-p path)
                                  (list path)
                                (let ((file-maybe (file-truename
                                                   (expand-file-name path (file-name-directory file-path)))))
                                  (if (f-exists? file-maybe)
                                      (list file-maybe)
                                    (list path))))))))
              (seq-do (lambda (name)
                        (when name
                          (push (vector file-path
                                        name
                                        type
                                        properties)
                                links)))
                      names))))))
    links))

(defun org-roam--extract-headlines (&optional file-path)
  "Extract all headlines with IDs within the current buffer.
If FILE-PATH is nil, use the current file."
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name)))))
    (org-element-map (org-element-parse-buffer) 'node-property
      (lambda (node-property)
        (let ((key (org-element-property :key node-property))
              (value (org-element-property :value node-property)))
          (when (string= key "ID")
            (let* ((id value)
                   (data (vector id
                                 file-path)))
              data)))))))

(defun org-roam--extract-titles-title ()
  "Return title from \"#+title\" of the current buffer."
  (let* ((prop (org-roam--extract-global-props '("TITLE")))
         (title (cdr (assoc "TITLE" prop))))
    (when title
      (list title))))

(defun org-roam--extract-titles-alias ()
  "Return the aliases from the current buffer.
Reads from the \"roam_alias\" property."
  (let* ((prop (org-roam--extract-global-props '("ROAM_ALIAS")))
         (aliases (cdr (assoc "ROAM_ALIAS" prop))))
    (condition-case nil
        (org-roam--str-to-list aliases)
      (error
       (progn
         (lwarn '(org-roam) :error
                "Failed to parse aliases for buffer: %s. Skipping"
                (or org-roam-file-name
                    (buffer-file-name)))
         nil)))))

(defun org-roam--extract-titles-headline ()
  "Return the first headline of the current buffer."
  (let ((headline (org-element-map
                      (org-element-parse-buffer)
                      'headline
                    (lambda (h)
                      (org-no-properties (org-element-property :raw-value h)))
                    :first-match t)))
    (when headline
      (list headline))))

(defun org-roam--extract-titles (&optional sources nested)
  "Extract the titles from current buffer using SOURCES.
If NESTED, return the first successful result from SOURCES."
  (let (coll res)
    (cl-dolist (source (or sources
                           org-roam-title-sources))
      (setq res (if (symbolp source)
                    (funcall (intern (concat "org-roam--extract-titles-" (symbol-name source))))
                  (org-roam--extract-titles source t)))
      (when res
        (if (not nested)
            (setq coll (nconc coll res))
          (setq coll res)
          (cl-return))))
    coll))

(defun org-roam--extract-tags-all-directories (file)
  "Extract tags from using the directory path FILE.
All sub-directories relative to `org-roam-directory' are used as tags."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (file-truename org-roam-directory)))))
    (f-split dir-relative)))

(defun org-roam--extract-tags-last-directory (file)
  "Extract tags from using the directory path FILE.
The final directory component is used as a tag."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (file-truename org-roam-directory)))))
    (last (f-split dir-relative))))

(defun org-roam--extract-tags-first-directory (file)
  "Extract tags from path FILE.
The first directory component after `org-roam-directory' is used as a
tag."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (file-truename org-roam-directory)))))
    (list (car (f-split dir-relative)))))

(defun org-roam--extract-tags-prop (_file)
  "Extract tags from the current buffer's \"#roam_tags\" global property."
  (let* ((prop (cdr (assoc "ROAM_TAGS" (org-roam--extract-global-props '("ROAM_TAGS"))))))
    (condition-case nil
        (org-roam--str-to-list prop)
      (error
       (progn
         (lwarn '(org-roam) :error
                "Failed to parse tags for buffer: %s. Skipping"
                (or org-roam-file-name
                    (buffer-file-name)))
         nil)))))

(defun org-roam--extract-tags (&optional file)
  "Extract tags from the current buffer.
If file-path FILE, use it to determine the directory tags.
Tags are obtained via:

1. Directory tags: Relative to `org-roam-directory': each folder
   path is considered a tag.
2. The key #+roam_tags."
  (let* ((file (or file (buffer-file-name (buffer-base-buffer))))
         (tags (mapcan (lambda (source)
                         (funcall (intern (concat "org-roam--extract-tags-"
                                                  (symbol-name source)))
                                  file))
                       org-roam-tag-sources)))
    (pcase org-roam-tag-sort
      ('nil tags)
      ((pred booleanp) (cl-sort tags 'string-lessp :key 'downcase))
      (`(,(pred symbolp) . ,_)
       (apply #'cl-sort (push tags org-roam-tag-sort)))
      (wrong-type (signal 'wrong-type-argument
                          `((booleanp (list symbolp))
                            ,wrong-type))))))

(defun org-roam--cite-prefix (ref)
  "Return the citation prefix of REF, or nil otherwise.
The prefixes are defined in `org-ref-cite-types`.
Examples:
   (org-roam--cite-prefix \"cite:foo\") -> \"cite:\"
   (org-roam--cite-prefix \"https://google.com\") -> nil"
  (when (require 'org-ref nil t)
    (seq-find
     (lambda (prefix) (s-prefix? prefix ref))
     (-map (lambda (type) (concat type ":"))
           org-ref-cite-types))))

(defun org-roam--ref-type (ref)
  "Determine the type of the REF from the prefix."
  (let* ((cite-prefix (org-roam--cite-prefix ref))
         (is-website (seq-some
                      (lambda (prefix) (s-prefix? prefix ref))
                      '("http" "https")))
         (type (cond (cite-prefix "cite")
                     (is-website "website")
                     (t "file"))))
    type))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer and return the type and the key of the ref."
  (pcase (cdr (assoc "ROAM_KEY"
                     (org-roam--extract-global-props '("ROAM_KEY"))))
    ('nil nil)
    ((pred string-empty-p)
     (user-error "Org property #+roam_key cannot be empty"))
    (ref
     (let* ((type (org-roam--ref-type ref))
            (key (cond ((string= "cite" type)
                        (s-chop-prefix (org-roam--cite-prefix ref) ref))
                       (t ref))))
       (cons type key)))))

(defun org-roam--ref-type-p (type)
  "Return t if the ref from current buffer is TYPE."
  (let ((current (car (org-roam--extract-ref))))
    (eq current type)))

;;;; Title/Path/Slug conversion
(defun org-roam--path-to-slug (path)
  "Return a slug from PATH."
  (-> path
      (file-relative-name (file-truename org-roam-directory))
      (file-name-sans-extension)))

(defun org-roam--get-title-or-slug (path)
  "Convert `PATH' to the file title, if it exists.  Else, return the path."
  (or (org-roam-db--get-titles path)
      (org-roam--path-to-slug path)))

(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(defun org-roam--format-link-title (title &optional type)
  "Return the link title, given the file TITLE.
If `org-roam-link-title-format title' is defined, use it with TYPE."
  (if (functionp org-roam-link-title-format)
      (funcall org-roam-link-title-format title type)
    (format org-roam-link-title-format title)))

(defun org-roam--format-link (target &optional description type)
  "Formats an org link for a given file TARGET, link DESCRIPTION and link TYPE.
TYPE defaults to \"file\"."
  (let* ((here (ignore-errors
                 (-> (or (buffer-base-buffer)
                         (current-buffer))
                     (buffer-file-name)
                     (file-truename)
                     (file-name-directory)))))
    (org-roam-link-make-string
     (concat (or type "file") ":" (if here
                                    (file-relative-name target here)
                                  target))
     description)))

(defun org-roam--get-title-path-completions ()
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)]))
         completions)
    (seq-sort-by (lambda (x)
                   (plist-get (nth 3 x) :mtime))
                 #'time-less-p
                 rows)
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (concat
                      (when tags
                        (format "(%s) " (s-join org-roam-tag-separator tags)))
                      title))
                  (v (list :path file-path :title title)))
              (push (cons k v) completions))))))

(defun org-roam--get-index-path ()
  "Return the path to the index in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, it is assumed to be a note in `org-roam-directory'
whose title is 'Index'."
  (let* ((index org-roam-index-file)
         (path (pcase index
                 ((pred functionp) (funcall index))
                 ((pred stringp) index)
                 ('nil (user-error "You need to set `org-roam-index-file' before you can jump to it"))
                 (wrong-type (signal 'wrong-type-argument
                                     `((functionp stringp)
                                       ,wrong-type))))))
    (if (f-relative-p index)
        (concat (file-truename org-roam-directory) path)
      index)))

;;;; org-roam-find-ref
(defun org-roam--get-ref-path-completions (&optional arg filter)
  "Return an alist of refs to absolute path of Org-roam files.

When called interactively (i.e. when ARG is 1), formats the car
of the completion-candidates with extra information: title, tags,
and type \(when `org-roam-include-type-in-ref-path-completions'
is non-nil).

When called with a `C-u' prefix (i.e. when ARG is 4), forces the
default format without the formatting.

FILTER can either be a string or a function:

- If it is a string, it should be the type of refs to include as
  candidates \(e.g. \"cite\", \"website\", etc.)

- If it is a function, it should be the name of a function that
  takes three arguments: the type, the ref, and the file of the
  current candidate. It should return t if that candidate is to
  be included as a candidate."
  (let ((rows (org-roam-db-query
               [:select [refs:type refs:ref refs:file titles:title tags:tags]
                :from titles
                :left :join tags
                :on (= titles:file tags:file)
                :left :join refs :on (= titles:file refs:file)
                :where refs:file :is :not :null]))
        completions)
    (seq-sort-by (lambda (x)
                   (plist-get (nth 3 x) :mtime))
                 #'time-less-p
                 rows)
    (dolist (row rows completions)
      (pcase-let ((`(,type ,ref ,file-path ,title ,tags) row))
        (when (pcase filter
                  ('nil t)
                  ((pred stringp) (string= type filter))
                  ((pred functionp) (funcall filter type ref file-path))
                  (wrong-type (signal 'wrong-type-argument
                                      `((stringp functionp)
                                        ,wrong-type))))
            (let ((k (if (eq arg 1)
                         (concat
                          (when org-roam-include-type-in-ref-path-completions
                            (format "{%s} " type))
                          (when tags
                            (format "(%s) " (s-join org-roam-tag-separator tags)))
                          (format "%s (%s)" title ref))
                       ref))
                  (v (list :path file-path :type type :ref ref)))
              (push (cons k v) completions)))))))

(defun org-roam--find-file (file)
  "Open FILE using `org-roam-find-file-function' or `find-file'."
  (funcall (or org-roam-find-file-function #'find-file) file))

(defun org-roam--find-ref (ref)
  "Find and open and Org-roam file from REF if it exists.
REF should be the value of '#+roam_key:' without any
type-information (e.g. 'cite:').
Return nil if the file does not exist."
  (when-let* ((completions (org-roam--get-ref-path-completions))
              (file (plist-get (cdr (assoc ref completions)) :path)))
    (org-roam--find-file file)))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam--file-path-from-id (id)
  "Return path for Org-roam file with ID."
  (file-truename
   (let* ((ext (or (car org-roam-file-extensions)
                   "org"))
          (file (concat id "." ext)))
     (expand-file-name
      (if org-roam-encrypt-files
          (concat file ".gpg")
        file)
      org-roam-directory))))

;;; org-roam-backlinks-mode
(define-minor-mode org-roam-backlinks-mode
  "Minor mode for the `org-roam-buffer'.
\\{org-roam-backlinks-mode-map}"
  :lighter " Backlinks"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] 'org-open-at-point)
             (define-key map (kbd "RET") 'org-open-at-point)
             map)
  (if org-roam-backlinks-mode
      (add-hook 'org-open-at-point-functions
                #'org-roam-open-at-point nil 'local)
    (remove-hook 'org-open-at-point-functions
                 #'org-roam-open-at-point 'local)))

(defun org-roam--in-buffer-p ()
  "Return t if in the Org-roam buffer."
  (and (boundp org-roam-backlinks-mode)
       org-roam-backlinks-mode))

(defun org-roam--retrieve-link-destination (&optional pom)
  "Retrieve the destination of the link at POM.
The point-or-marker POM can either be a position in the current
buffer or a marker."
  (let ((pom (or pom (point))))
    (org-with-point-at pom
      (let* ((context (org-element-context))
             (type (org-element-property :type context))
             (dest (org-element-property :path context)))
        (pcase type
          ("id" (car (org-roam-id-find dest)))
          (_ dest))))))

(defun org-roam--backlink-to-current-p ()
  "Return t if backlink is to the current Org-roam file."
  (let ((current (buffer-file-name org-roam-buffer--current))
        (backlink-dest (org-roam--retrieve-link-destination)))
    (string= current backlink-dest)))

(defun org-roam-open-at-point ()
  "Open an Org-roam link or visit the text previewed at point.
When point is on an Org-roam link, open the link in the Org-roam window.
When point is on the Org-roam preview text, open the link in the Org-roam
window, and navigate to the point.
This function hooks into `org-open-at-point' via `org-open-at-point-functions'."
  (cond
   ;; Org-roam link
   ((let* ((context (org-element-context))
           (path (org-element-property :path context)))
      (when (and (eq (org-element-type context) 'link)
                 (org-roam--org-roam-file-p (file-truename path)))
        (org-roam-buffer--find-file path)
        (org-show-context)
        t)))
   ;; Org-roam preview text
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (org-roam-buffer--find-file file-from)
      (goto-char p)
      (org-show-context)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(defun org-roam--get-backlinks (targets)
  "Return the backlinks for TARGETS.
TARGETS is a list of strings corresponding to the TO value in the
Org-roam cache. It may be a file, for Org-roam file links, or a
citation key, for Org-ref cite links."
  (unless (listp targets)
    (setq targets (list targets)))
  (let ((conditions (--> targets
                         (mapcar (lambda (i) (list '= 'to i)) it)
                         (org-roam--list-interleave it :or))))
    (org-roam-db-query `[:select [from to properties] :from links
                         :where ,@conditions
                         :order-by (asc from)])))

(defun org-roam-store-link ()
  "Store a link to an Org-roam file or heading."
  (when (and (bound-and-true-p org-roam-mode)
             (org-roam--org-roam-file-p))
    (if (org-before-first-heading-p)
        (when-let ((titles (org-roam--extract-titles)))
          (org-roam-link-store-props
           :type        "file"
           :link        (format "file:%s" (abbreviate-file-name buffer-file-name))
           :description (car titles)))
      (let ((id (org-id-get)))
          (org-id-store-link)
          ;; If :ID: was created, update the cache
          (unless id
            (org-roam-db--update-headlines))))))

(defun org-roam-id-find (id &optional markerp strict)
  "Return the location of the entry with the id ID.
When MARKERP is non-nil, return a marker pointing to theheadline.
Otherwise, return a cons formatted as \(file . pos).
When STRICT is non-nil, only consider Org-roam’s database."
  (let ((file (or (caar (org-roam-db-query [:select [file]
                                            :from headlines
                                            :where (= id $s1)]
                                           id))
                  (unless strict
                    (org-id-find-id-file id)))))
    (when file
      (org-id-find-id-in-file id file markerp))))

(defun org-roam-id-open (id-or-marker &optional strict)
  "Go to the entry with ID-OR-MARKER.
Wrapper for `org-id-open' which tries to find the ID in the
Org-roam's database.
ID-OR-MARKER can either be the ID of the entry or the marker
pointing to it if it has already been computed by
`org-roam-id-find'. If the ID-OR-MARKER is not found, it reverts
to the default behaviour of `org-id-open'.
When STRICT is non-nil, only consider Org-roam’s database."
  (when-let ((marker (if (markerp id-or-marker)
                         id-or-marker
                       (org-roam-id-find id-or-marker t strict))))
    (org-goto-marker-or-bmk marker)
    (set-marker marker nil)))

(defun org-roam-open-id-at-point ()
  "Open link, timestamp, footnote or tags at point.
The function tries to open ID-links with Org-roam’s database
before falling back to the default behaviour of
`org-open-at-point'. It also asks the user whether to parse
`org-id-files' when an ID is not found because it might be a slow
process.
This function hooks into `org-open-at-point' via
`org-open-at-point-functions'."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      (cond ((org-roam-id-open id)
             t)
            ;; Ask whether to parse `org-id-files'
            ((not (y-or-n-p (concat "ID was not found in `org-roam-directory' nor in `org-id-locations'.\n"
                                    "Search in `org-id-files'? ")))
             t)
            ;; Conditionally fall back to default behaviour
            (t
             nil)))))

;;; Completion at point
(defconst org-roam-fuzzy-link-regexp
  (rx (seq "[["
           (group
            (zero-or-more
             (or (not (any "[]\\"))
                 (and "\\" (zero-or-more "\\\\") (any "[]"))
                 (and (one-or-more "\\") (not (any "[]"))))))
           "]]")))

(defun org-roam-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (cond
     (;; completing roam_tags
      (looking-back "^#\\+roam_tags:.*" (line-beginning-position))
      (when (looking-at "\\>")
        (setq start (save-excursion (skip-syntax-backward "w")
                                    (point))
              end (point)))
      (setq collection #'org-roam-db--get-tags
            exit-fn (lambda (str _status)
                      (delete-char (- (length str)))
                      (insert "\"" str "\""))))
     (;; In a fuzzy link
      (org-in-regexp org-roam-fuzzy-link-regexp)
      (setq start (match-beginning 1)
            end (match-end 1))
      (pcase-let ((`(,type ,title _ ,star-idx)
                   (org-roam--split-fuzzy-link (match-string-no-properties 1))))
        (pcase type
          ('title+headline
           (when-let ((file (org-roam--get-file-from-title title t)))
             (setq collection (apply-partially #'org-roam--get-headlines file))
             (setq start (+ start star-idx 1))))
          ('title
           (setq collection #'org-roam--get-titles))
          ('headline
           (setq collection #'org-roam--get-headlines)
           (setq start (+ start star-idx 1)))))))
      (when collection
        (let ((prefix (buffer-substring-no-properties start end)))
          (list start end
                (if (functionp collection)
                    (completion-table-dynamic
                     (lambda (_)
                       (cl-remove-if (apply-partially 'string= prefix) (funcall collection))))
                  collection)
                :exit-function exit-fn)))))

;;; Fuzzy Links
(defcustom org-roam-auto-replace-fuzzy-links t
  "When t, replace Org-roam's fuzzy links with file or id links whenever possible."
  :group 'org-roam
  :type 'boolean)

(defun org-roam--split-fuzzy-link (link)
  "Splits LINK into title and headline.
Return a list of the form (type title has-headline-p headline star-idx).
type is one of `title', `headline', `title+headline'.
title is the title component of the link.
headline is the headline component of the link.
star-idx is the index of the asterisk, if any."
  (save-match-data
    (let* ((star-index (string-match-p "\\*" link))
           (title (substring-no-properties link 0 star-index))
           (headline (if star-index
                         (substring-no-properties link (+ 1 star-index))
                       ""))
           (type (cond ((not star-index)
                       'title)
                       ((= 0 star-index)
                       'headline)
                       (t 'title+headline))))
      (list type title headline star-index))))

(defun org-roam--get-titles ()
  "Return all titles within Org-roam."
  (mapcar #'car (org-roam-db-query [:select [titles:title] :from titles])))

(defun org-roam--get-headlines (&optional file with-marker use-stack)
  "Return all outline headings for the current buffer.
If FILE, return outline headings for passed FILE instead.
If WITH-MARKER, return a cons cell of (headline . marker).
If USE-STACK, include the parent paths as well."
  (let* ((buf (or (and file
                       (or (find-buffer-visiting file)
                           (find-file-noselect file)))
                  (current-buffer)))
         (bol-regex (concat "^\\(?:" outline-regexp "\\)"))
         (outline-title-fn (lambda () (buffer-substring-no-properties (point) (line-end-position))))
         (outline-level-fn outline-level)
         (path-separator "/")
         (stack-level 0)
         stack cands name level marker)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward bol-regex nil t)
          (save-excursion
            (setq name (or (save-match-data
                             (funcall outline-title-fn))
                           ""))
            (setq marker (point-marker))
            (when use-stack
              (goto-char (match-beginning 0))
              (setq level (funcall outline-level-fn))
              ;; Update stack.  The empty entry guards against incorrect
              ;; headline hierarchies, e.g. a level 3 headline
              ;; immediately following a level 1 entry.
              (while (<= level stack-level)
                (pop stack)
                (cl-decf stack-level))
              (while (> level stack-level)
                (push name stack)
                (cl-incf stack-level))
              (setq name (mapconcat #'identity
                                    (reverse stack)
                                    path-separator)))
            (push (if with-marker
                      (cons name marker)
                    name) cands)))))
    (nreverse cands)))

(defun org-roam--get-file-from-title (title &optional no-interactive)
  "Return the file path corresponding to TITLE.
When NO-INTERACTIVE, return nil if there are multiple options."
  (let ((files (mapcar #'car (org-roam-db-query [:select [titles:file] :from titles
                                                   :where (= titles:title $v1)]
                                                  (vector title)))))
    (pcase files
      ('nil nil)
      (`(,file) file)
      (_
       (unless no-interactive
         (completing-read "Select file: " files))))))

(defun org-roam--get-id-from-headline (headline &optional file)
  "Return (marker . id) correspondng to HEADLINE.
If FILE, get headline from FILE instead.
If there is no corresponding headline, return nil."
  (save-excursion
    (with-current-buffer (or (and file
                                  (or (find-buffer-visiting file)
                                      (find-file-noselect file)))
                             (current-buffer))
      (let ((headlines (org-roam--get-headlines file 'with-markers)))
        (when-let ((marker (cdr (assoc-string headline headlines))))
          (goto-char marker)
          (cons marker
                (when org-roam-auto-replace-fuzzy-links
                  (org-id-get-create))))))))

(defun org-roam--get-fuzzy-link-location (link)
  "Return the location of Org-roam fuzzy LINK.
The location is returned as a list containing (link-type loc desc marker).
nil is returned if there is no matching location.

link-type is either \"file\" or \"id\".
loc is the target location: e.g. a file path, or an id.
marker is a marker to the headline, if applicable."
  (let (mkr link-type desc loc)
    (pcase-let ((`(,type ,title ,headline _) (org-roam--split-fuzzy-link link)))
      (pcase type
        ('title+headline
         (let ((file (org-roam--get-file-from-title title)))
                 (if (not file)
                     (org-roam-message "Cannot find matching file")
                   (setq mkr (org-roam--get-id-from-headline headline file))
                   (pcase mkr
                     (`(,marker . ,target-id)
                      (setq mkr marker
                            loc target-id
                            link-type "id"
                            desc headline))
                     (_ (org-roam-message "cannot find matching id"))))))
        ('title
         (setq loc (org-roam--get-file-from-title title)
                     desc title
                     link-type "file")
         (when loc (setq loc (file-relative-name loc))))
        ('headline
         (setq mkr (org-roam--get-id-from-headline headline))
         (pcase mkr
                 (`(,marker . ,target-id)
                  (setq mkr marker
                        loc target-id
                        desc headline
                        link-type "id"))
                 (_ (org-roam-message "Cannot find matching headline")))))
      (list link-type loc desc mkr))))

(defun org-roam--open-fuzzy-link (link)
  "Open a Org fuzzy LINK.
To be added to `org-open-link-functions'. This function always
resolves, completely replacing Org's original fuzzy link opening behaviour.

Three types of fuzzy links are supported:

  [[Title]]
    Opens a file with the corresponding title.

  [[*Headline]]
    Creates or gets an ID for the corresponding headline from current file.

  [[Title*Headline]]
    Creates or gets an ID for the corresponding headline from file with corresponding title."
  (when (and (bound-and-true-p org-roam-mode)
             (org-roam--org-roam-file-p))
    (when-let ((location (org-roam--get-fuzzy-link-location link)))
      (pcase-let ((`(,link-type ,loc ,desc ,mkr) location))
        (when (and org-roam-auto-replace-fuzzy-links
                   loc desc)
          (org-roam-replace-fuzzy-link (concat link-type ":" loc) desc))
        (pcase link-type
          ("file"
           (if loc
               (org-roam--find-file loc)
             (org-roam-find-file desc nil nil t)))
          ("id"
           (org-goto-marker-or-bmk mkr)))))
    t))

(defun org-roam--replace-all-fuzzy-links ()
  "Replace all fuzzy links in current buffer."
  (save-excursion
    (let ((fuzzies (org-element-map (org-element-parse-buffer) 'link
                     (lambda (l)
                       (when (equal (org-element-property :type l)
                                    "fuzzy")
                         (cons (set-marker (make-marker) (org-element-property :begin l))
                               (org-element-property :path l)))))))
        (dolist (f fuzzies)
          (goto-char (car f))
          (when-let ((location (org-roam--get-fuzzy-link-location (cdr f))))
            (pcase-let ((`(,link-type ,loc ,desc _) location))
              (when (and link-type loc)
                (org-roam-replace-fuzzy-link (concat link-type ":" loc) desc))))))))

(defun org-roam--replace-fuzzy-link-on-save ()
  "Hook to replace all fuzzy links on save."
  (when org-roam-auto-replace-fuzzy-links
    (org-roam--replace-all-fuzzy-links)))

;;; Org-roam-mode
;;;; Function Faces
;; These faces are used by `org-link-set-parameters', which take one argument,
;; which is the path.
(defcustom org-roam-link-use-custom-faces 'everywhere
  "Define where to apply custom faces to Org-roam links.

Valide values are:

t            Use custom faces inside Org-roam notes (i.e. files in
             `org-roam-directory'.)

everywhere   Apply custom faces everywhere.

Otherwise, do not apply custom faces to Org-roam links."
  :type '(choice
	  (const :tag "Use custom faces inside Org-roam notes" t)
	  (const :tag "Apply custom faces everywhere" everywhere)
	  (const :tag "Do not apply custom faces" nil))
  :group 'org-roam)

(defun org-roam--file-link-face (path)
  "Conditional face for file: links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                     (org-roam--org-roam-file-p)))
         (custom (or (and in-note org-roam-link-use-custom-faces)
                     (eq org-roam-link-use-custom-faces 'everywhere))))
    (cond ((and custom
                (not (file-remote-p path)) ;; Prevent lockups opening Tramp links
                (not (file-exists-p path)))
           'org-roam-link-invalid)
          ((and (org-roam--in-buffer-p)
                (org-roam--backlink-to-current-p))
           'org-roam-link-current)
          ((and custom
                (org-roam--org-roam-file-p path))
           'org-roam-link)
          (t
           'org-link))))

(defun org-roam--id-link-face (id)
  "Conditional face for id links.
Applies `org-roam-link-current' if ID corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if ID corresponds to any other Org-roam
file."
  (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                     (org-roam--org-roam-file-p)))
         (custom (or (and in-note org-roam-link-use-custom-faces)
                     (eq org-roam-link-use-custom-faces 'everywhere))))
    (cond ((and custom
                (not (org-roam-id-find id)))
           'org-roam-link-invalid)
          ((and (org-roam--in-buffer-p)
                (org-roam--backlink-to-current-p))
           'org-roam-link-current)
          ((and custom
                (org-roam-id-find id))
           'org-roam-link)
          (t
           'org-link))))

(defun org-roam--queue-file-for-update (&optional file-path)
  "Queue FILE-PATH for `org-roam' database update.
This is a lightweight function that is called during `after-save-hook'
and only schedules the current Org file to be `org-roam' updated
during the next idle slot."
  (let ((fp (or file-path buffer-file-name)))
    (when (org-roam--org-roam-file-p file-path)
      (add-to-list 'org-roam--file-update-queue fp))))

(defun org-roam--process-update-queue ()
  "Process files queued in `org-roam--file-update-queue'."
  (when org-roam--file-update-queue
    (mapc #'org-roam-db--update-file org-roam--file-update-queue)
    (org-roam-message "database updated during idle: %s."
                      (mapconcat #'file-name-nondirectory org-roam--file-update-queue  ", ") )
    (setq org-roam--file-update-queue nil)))

;;;; Hooks and Advices
(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when mode symbol `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam-buffer--update-maybe nil t)
    (add-hook 'before-save-hook #'org-roam--replace-fuzzy-link-on-save nil t)
    (add-hook 'after-save-hook #'org-roam--queue-file-for-update nil t)
    (add-hook 'completion-at-point-functions #'org-roam-complete-at-point nil t)
    (org-roam-buffer--update-maybe :redisplay t)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (org-roam-db--clear-file (file-truename file))))

(defun org-roam--replace-link (file old-path new-path &optional old-desc new-desc)
  "Replace Org-roam file links in FILE with path OLD-PATH to path NEW-PATH.
If OLD-DESC is passed, and is not the same as the link
description, it is assumed that the user has modified the
description, and the description will not be updated. Else,
update with NEW-DESC."
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (save-excursion
      (let ((link-markers (org-element-map (org-element-parse-buffer) 'link
                            (lambda (l)
                              (let ((type (org-element-property :type l))
                                    (path (org-element-property :path l)))
                                (when (string-equal (file-truename path)
                                                    old-path)
                                  (cons (set-marker (make-marker) (org-element-property :begin l))
                                        type)))))))
        (dolist (m link-markers)
          (goto-char (car m))
          (save-match-data
            (unless (org-in-regexp org-link-bracket-re 1)
              (user-error "No link at point"))
            (let* ((label (if (match-end 2)
                              (match-string-no-properties 2)
                            (org-link-unescape (match-string-no-properties 1))))
                   (new-label (if (string-equal label old-desc)
                                  new-desc
                                label)))
              (replace-match (org-roam-link-make-string
                              (concat (cdr m) ":"
                                      (file-relative-name new-path (file-name-directory (buffer-file-name))))
                              new-label)))))))
    (save-buffer)))

(defun org-roam--fix-relative-links (old-path)
  "Fix file-relative links in current buffer.
File relative links are assumed to originate from OLD-PATH. The
replaced links are made relative to the current buffer."
  (let* ((links (org-element-map (org-element-parse-buffer) 'link
                  (lambda (link)
                    (let ((type (org-element-property :type link))
                          (path (org-element-property :path link)))
                      (when (f-relative-p path)
                        (cons (set-marker (make-marker)
                                          (org-element-property :begin link))
                              (cons path type))))))))
    (save-excursion
      (save-match-data
        (dolist (link links)
          (pcase-let ((`(,marker . (,path . ,type)) link))
            (goto-char marker)
            (unless (org-in-regexp org-link-bracket-re 1)
              (user-error "No link at point"))
            (let* ((file-path (expand-file-name path (file-name-directory old-path)))
                   (new-path (file-relative-name file-path (file-name-directory (buffer-file-name)))))
              (replace-match (concat type ":" new-path)
                             nil t nil 1))
            (set-marker marker nil)))))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Rename backlinks of OLD-FILE to refer to NEW-FILE-OR-DIR."
  ;; When rename-file is passed a directory as an argument, compute the new name
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam--org-roam-file-p old-file))
      (org-roam-db--ensure-built)
      (let* ((old-path (file-truename old-file))
             (new-path (file-truename new-file))
             (old-slug (org-roam--get-title-or-slug old-file))
             (old-desc (org-roam--format-link-title old-slug))
             (new-slug (or (org-roam-db--get-titles old-path)
                           (org-roam--path-to-slug new-path)))
             (new-desc (org-roam--format-link-title new-slug))
             (new-buffer (or (find-buffer-visiting new-path)
                             (find-file-noselect new-path)))
             (files-to-rename (org-roam-db-query [:select :distinct [from]
                                                  :from links
                                                  :where (= to $s1)]
                                                 old-path)))
        ;; Remove database entries for old-file.org
        (org-roam-db--clear-file old-file)
        ;; Insert new headlines locations in new-file.org after removing the previous IDs
        (with-current-buffer new-buffer
          (org-roam-db--update-headlines))
        ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
        (mapc (lambda (file)
                (setq file (if (string-equal (file-truename (car file)) old-path)
                               new-path
                             (car file)))
                (org-roam--replace-link file old-path new-path old-desc new-desc)
                (org-roam-db--update-file file))
              files-to-rename)
        ;; If the new path is in a different directory, relative links
        ;; will break. Fix all file-relative links:
        (unless (string= (file-name-directory old-path)
                         (file-name-directory new-path))
          (with-current-buffer new-buffer
            (org-roam--fix-relative-links old-path)
            (save-buffer)))
        (when (org-roam--org-roam-file-p new-file)
          (org-roam-db--update-file new-path))))))

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
  :keymap  (let ((map (make-sparse-keymap)))
             map)
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (unless (or (and (boundp 'emacsql-sqlite3-executable)
                     (file-executable-p emacsql-sqlite3-executable))
                (executable-find "sqlite3"))
      (lwarn '(org-roam) :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at Org-roam > Installation > Post-Installation Tasks."))
    (add-to-list 'org-execute-file-search-functions 'org-roam--execute-file-row-col)
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (add-hook 'org-open-at-point-functions #'org-roam-open-id-at-point)
    (add-hook 'org-open-link-functions #'org-roam--open-fuzzy-link)
    (unless org-roam--file-update-timer
      (setq org-roam--file-update-timer (run-with-idle-timer 2 t #'org-roam--process-update-queue)))
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice)
    (when (fboundp 'org-link-set-parameters)
      (when org-roam-enable-headline-linking
        (org-link-set-parameters "file" :face 'org-roam--file-link-face :store #'org-roam-store-link))
      (org-link-set-parameters "id" :face 'org-roam--id-link-face))
    (org-roam-db-build-cache))
   (t
    (setq org-execute-file-search-functions (delete 'org-roam--execute-file-row-col org-execute-file-search-functions))
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (remove-hook 'org-open-at-point-functions #'org-roam-open-id-at-point)
    (remove-hook 'org-open-link-functions #'org-roam--open-fuzzy-link)
    (when org-roam--file-update-timer
      (cancel-timer org-roam--file-update-timer))
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    (when (fboundp 'org-link-set-parameters)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))
    (org-roam-db--close-all)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'org-roam-buffer--update-maybe t)
        (remove-hook 'before-save-hook #'org-roam--replace-fuzzy-link-on-save t)
        (remove-hook 'after-save-hook #'org-roam--queue-file-for-update t))))))

;;; Interactive Commands
;;;###autoload
(defalias 'org-roam 'org-roam-buffer-toggle-display)

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

;;;###autoload
(defun org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

;;;###autoload
(defun org-roam-find-directory ()
  "Find and open `org-roam-directory'."
  (interactive)
  (org-roam--find-file org-roam-directory))

;;;###autoload
(defun org-roam-find-ref (arg &optional filter)
  "Find and open an Org-roam file from a ref.
ARG is used to forward interactive calls to
`org-roam--get-ref-path-completions'
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate."
  (interactive "p")
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (org-roam--get-ref-path-completions arg filter))
         (ref (org-roam-completion--completing-read "Ref: "
                                                    completions
                                                    :require-match t))
         (file (-> (cdr (assoc ref completions))
                   (plist-get :path))))
    (org-roam--find-file file)))

;;;###autoload
(defun org-roam-random-note ()
  "Find a random Org-roam file."
  (interactive)
  (find-file (seq-random-elt (org-roam--list-all-files))))

;;;###autoload
(defun org-roam-insert (&optional lowercase completions filter-fn description link-type)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
If LOWERCASE, downcase the title before insertion.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (buffer-substring-no-properties beg end))))
               (completions (--> (or completions
                                     (org-roam--get-title-path-completions))
                                 (if filter-fn
                                     (funcall filter-fn it)
                                   it)))
               (title-with-tags (org-roam-completion--completing-read "File: " completions
                                                                      :initial-input region-text))
               (res (cdr (assoc title-with-tags completions)))
               (title (or (plist-get res :title)
                          title-with-tags))
               (target-file-path (plist-get res :path))
               (description (or description region-text title))
               (link-description (org-roam--format-link-title (if lowercase
                                                                  (downcase description)
                                                                description)
                                                              link-type)))
          (cond ((and target-file-path
                      (file-exists-p target-file-path))
                 (when region-text
                   (delete-region beg end)
                   (set-marker beg nil)
                   (set-marker end nil))
                 (insert (org-roam--format-link target-file-path link-description link-type)))
                (t
                 (let ((org-roam-capture--info `((title . ,title-with-tags)
                                                 (slug . ,(funcall org-roam-title-to-slug-function title-with-tags))))
                       (org-roam-capture--context 'title))
                   (setq org-roam-capture-additional-template-props (list :region (org-roam-shield-region beg end)
                                                                          :insert-at (point-marker)
                                                                          :link-type link-type
                                                                          :link-description link-description
                                                                          :finalize 'insert-link))
                   (org-roam-capture--capture))))
          res))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-insert-immediate (arg &rest args)
  "Find an Org-roam file, and insert a relative org link to it at point.
This variant of `org-roam-insert' inserts the link immediately by
using the template in `org-roam-capture-immediate-template'. The
interactive ARG and ARGS are passed to `org-roam-insert'.
See `org-roam-insert' for details."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list org-roam-capture-immediate-template)))
    (apply #'org-roam-insert args)))

;;;###autoload
(defun org-roam-find-file-immediate (arg &rest args)
  "Find and open an Org-roam file.
This variant of `org-roam-find-file' uses the template in
`org-roam-capture-immediate-template', avoiding the capture
process. The interactive ARG and ARGS are passed to
`org-roam-find-file'. See `org-roam-find-file' for details."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list org-roam-capture-immediate-template)))
    (apply #'org-roam-find-file args)))

;;;###autoload
(defun org-roam-jump-to-index ()
  "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((index (org-roam--get-index-path)))
    (if (and index
             (file-exists-p index))
        (org-roam--find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (org-roam-find-file "Index")))))

;;;###autoload
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
    (when-let ((name (org-roam-completion--completing-read "Buffer: " names-and-buffers
                                                           :require-match t)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

(defun org-roam--execute-file-row-col (s)
  "Move to row:col if S match the row:col syntax. To be used with `org-execute-file-search-functions'."
  (when (string-match (rx (group (1+ digit))
                          ":"
                          (group (1+ digit))) s)
    (let ((row (string-to-number (match-string 1 s)))
          (col (string-to-number (match-string 2 s))))
      (org-goto-line row)
      (move-to-column (- col 1))
      t)))

;;###autoload
(defun org-roam-unlinked-references ()
  "Check for unlinked references in the current buffer.

The check here is naive: it uses a regex that detects for
strict (case-insensitive) occurrences of possible titles (see
`org-roam--extract-titles'), and shows them in a buffer. This
means that the results can be noisy, and may not truly indicate
an unlinked reference.

Users are encouraged to think hard about whether items should be
linked, lest the network graph get too crowded."
  (interactive)
  (unless (org-roam--org-roam-file-p)
    (user-error "Not in org-roam file"))
  (if (not (executable-find "rg"))
      (error "Cannot find the ripgrep executable \"rg\". Check that it is installed and available on `exec-path'")
    (let* ((titles (org-roam--extract-titles))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                    (org-roam--list-files-search-globs org-roam-file-extensions)) " ")
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-roam-directory))
           (file-loc (buffer-file-name))
           (buf (get-buffer-create "*org-roam unlinked references*"))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           (result-regex (rx (group (one-or-more anything))
                             ":"
                             (group (one-or-more digit))
                             ":"
                             (group (one-or-more digit))
                             ":"
                             (group (zero-or-more anything)))))
      (pop-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (propertize (car titles) 'font-lock-face 'org-document-title) "\n\n"
                "* Unlinked References\n")
        (dolist (line results)
          (save-match-data
            (when (string-match result-regex line)
              (let ((file (match-string 1 line))
                    (row (match-string 2 line))
                    (col (match-string 3 line))
                    (match (match-string 4 line)))
                (when (and match
                           (member (downcase match) (mapcar #'downcase titles))
                           (not (f-equal-p (expand-file-name file org-roam-directory)
                                           file-loc)))
                  (let ((rowcol (concat row ":" col)))
                    (insert "- "
                            (org-roam-link-make-string (concat "file:" file "::" rowcol)
                                                  (format "[%s] %s" rowcol (org-roam--get-title-or-slug file))))
                    (when (executable-find "sed") ; insert line contents when sed is available
                      (insert " :: "
                              (shell-command-to-string
                               (concat "sed -n "
                                       row
                                       "p "
                                       file))))
                    (insert "\n")))))))
        (read-only-mode +1)
        (dolist (title titles)
          (highlight-phrase (downcase title) 'bold-italic))
        (goto-char (point-min))))))


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

(provide 'org-roam)
;;; org-roam.el ends here
