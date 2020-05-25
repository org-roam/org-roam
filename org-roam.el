;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.1.1
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
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'org-ref nil t) ; To detect cite: links
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
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
(require 'org-roam-buffer)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-dailies)
(require 'org-roam-db)
(require 'org-roam-doctor)
(require 'org-roam-graph)

;;;; Declarations
(defvar org-ref-cite-types) ;; from org-ref-core.el
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))

;;;; Customizable variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://org-roam.github.io/org-roam/manual/"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

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
    Extract tags from the #+ROAM_TAGS property.
    Tags are space delimited.
    Tags may contain spaces if they are double-quoted.
    e.g. #+ROAM_TAGS: tag \"tag with spaces\"

  `all-directories'
    Extract sub-directories relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tags \"foo\" and \"bar\".

  `last-directory'
    Extract the last directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"bar\"."
  :type '(set (const :tag "#+ROAM_TAGS" prop)
              (const :tag "sub-directories" all-directories)
              (const :tag "parent directory" last-directory)))

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
1. 'title: The \"#+TITLE\" property of org file.
2. 'alias: The \"#+ROAM_ALIAS\" property of the org file, using
space-delimited strings.
3. 'headline: The first headline in the org file."
  :type '(repeat
          (choice
           (repeat symbol)
           (symbol)))
  :group 'org-roam)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

;;;; Dynamic variables
(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defvar org-roam--org-link-file-bracket-re
  "\\[\\[file:\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\(\\(?:.\\|
\\)+?\\)]\\)?]"
  "Matches a 'file:' link in double brackets.")

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
  "Function to transform string STR into list of titles.

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
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory))))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (seq-filter #'s-present? (split-string (shell-command-to-string cmd) "\n")))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (append
   (mapcar (lambda (ext) (s-wrap (concat "*." ext) "\"")) exts)
   (mapcar (lambda (ext) (s-wrap (concat "*." ext ".gpg") "\"")) exts)))

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable ,dir "-type f \\("
                                            ,(s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)) "\\)"))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat #'regexp-quote org-roam-file-extensions "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (directory-files-recursively dir regex) result)
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
    (if path
        (let ((fn (intern (concat "org-roam--list-files-" exe))))
          (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
          (mapcar #'ansi-color-filter-apply
                  (funcall fn path (format "\"%s\"" dir))))
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
                   (when (string= (org-element-property :key kw) prop)
                     (org-element-property :value kw)))
                 :first-match t)))
        (push (cons prop p) res)))
    res))

(defun org-roam--expand-links (content path)
  "Crawl CONTENT for relative links and expand them.
PATH should be the root from which to compute the relativity."
  (let ((dir (file-name-directory path))
        (re org-roam--org-link-file-bracket-re)
        link)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Loop over links
      (while (re-search-forward re (point-max) t)
        (goto-char (match-beginning 1))
        ;; Strip 'file:'
        (setq link (match-string 1))
        ;; Delete relative link
        (when (f-relative-p link)
          (delete-region (match-beginning 1)
                         (match-end 1))
          (insert (expand-file-name
                   (concat dir link)))))
      (buffer-string))))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [from to type properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name))))
        links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let* ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (start (org-element-property :begin link))
               (link-type (cond ((and (string= type "file")
                                      (org-roam--org-file-p path))
                                 "roam")
                                ((and
                                  (require 'org-ref nil t)
                                  (-contains? org-ref-cite-types type))
                                 "cite")
                                (t nil))))
          (when link-type
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
              (let ((context (list :content content :point begin))
                    (names (pcase link-type
                             ("roam"
                              (list (file-truename (expand-file-name path (file-name-directory file-path)))))
                             ("cite"
                              (org-ref-split-and-strip-string path)))))
                (seq-do (lambda (name)
                          (push (vector file-path
                                        name
                                        link-type
                                        context)
                                links))
                        names)))))))
    links))

(defun org-roam--extract-titles-title ()
  "Return title from \"#+TITLE\" of the current buffer."
  (let* ((prop (org-roam--extract-global-props '("TITLE")))
         (title (cdr (assoc "TITLE" prop))))
    (when title
      (list title))))

(defalias 'org-roam--parse-alias 'org-roam--str-to-list)

(defun org-roam--extract-titles-alias ()
  "Return the aliases from the current buffer.
Reads from the \"ROAM_ALIAS\" property."
  (let* ((prop (org-roam--extract-global-props '("ROAM_ALIAS")))
         (aliases (cdr (assoc "ROAM_ALIAS" prop))))
    (org-roam--parse-alias aliases)))

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
                            (file-relative-name file org-roam-directory))))
    (f-split dir-relative)))

(defun org-roam--extract-tags-last-directory (file)
  "Extract tags from using the directory path FILE.
The final directory component is used as a tag."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file org-roam-directory))))
    (last (f-split dir-relative))))

(defalias 'org-roam--parse-tags 'org-roam--str-to-list)

(defun org-roam--extract-tags-prop (_file)
  "Extract tags from the current buffer's \"#ROAM_TAGS\" global property."
  (let* ((prop (org-roam--extract-global-props '("ROAM_TAGS"))))
    (org-roam--parse-tags (cdr (assoc "ROAM_TAGS" prop)))))

(defun org-roam--extract-tags (&optional file)
  "Extract tags from the current buffer.
If file-path FILE, use it to determine the directory tags.
Tags are obtained via:

1. Directory tags: Relative to `org-roam-directory': each folder
   path is considered a tag.
2. The key #+ROAM_TAGS."
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
                          `((booleanp (list symbolp …))
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
                     (t "roam"))))
    type))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer and return the type and the key of the ref."
  (pcase (cdr (assoc "ROAM_KEY"
                     (org-roam--extract-global-props '("ROAM_KEY"))))
    ('nil nil)
    ((pred string-empty-p)
     (user-error "ROAM_KEY cannot be empty"))
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
  (or (car (org-roam-db--get-titles path))
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

(defun org-roam--format-link-title (title)
  "Return the link title, given the file TITLE."
  (if (functionp org-roam-link-title-format)
      (funcall org-roam-link-title-format title)
    (format org-roam-link-title-format title)))

(defun org-roam--format-link (target &optional description)
  "Formats an org link for a given file TARGET and link DESCRIPTION."
  (let* ((here (ignore-errors
                 (-> (or (buffer-base-buffer)
                         (current-buffer))
                     (buffer-file-name)
                     (file-truename)
                     (file-name-directory)))))
    (org-link-make-string
     (concat "file:" (if here
                         (file-relative-name target here)
                       target))
     description)))

(defun org-roam--get-title-path-completions ()
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* ((rows (org-roam-db-query [:select [titles:file titles:titles tags:tags files:meta] :from titles
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
      (pcase-let ((`(,file-path ,titles ,tags) row))
        (let ((titles (or titles (list (org-roam--path-to-slug file-path)))))
          (dolist (title titles)
            (let ((k (concat
                      (when tags
                        (format "(%s) " (s-join org-roam-tag-separator tags)))
                      title))
                  (v (list :path file-path :title title)))
              (push (cons k v) completions))))))))

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
(defun org-roam--get-ref-path-completions (&optional interactive filter)
  "Return a alist of refs to absolute path of Org-roam files.
When `org-roam-include-type-in-ref-path-completions' and
INTERACTIVE are non-nil, format the car of the
completion-candidates as 'type:ref'.
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate."
  (let ((rows (org-roam-db-query [:select [refs:type refs:ref refs:file ] :from refs
                                  :left :join files
                                  :on (= refs:file files:file)]))
        (include-type (and interactive
                           org-roam-include-type-in-ref-path-completions))
        completions)
    (seq-sort-by (lambda (x)
                   (plist-get (nth 3 x) :mtime))
                 #'time-less-p
                 rows)
    (dolist (row rows completions)
      (pcase-let ((`(,type ,ref ,file-path) row))
        (when (pcase filter
                ('nil t)
                ((pred stringp) (string= type filter))
                ((pred functionp) (funcall filter type ref file-path))
                (wrong-type (signal 'wrong-type-argument
                                    `((stringp functionp)
                                      ,wrong-type))))
          (let ((k (concat
                    (when include-type
                      (format "(%s) " type))
                    ref))
                (v (list :path file-path :type type :ref ref)))
            (push (cons k v) completions)))))))

(defun org-roam--find-ref (ref)
  "Find and open and Org-roam file from REF if it exists.
REF should be the value of '#+ROAM_KEY:' without any
type-information (e.g. 'cite:').
Return nil if the file does not exist."
  (when-let* ((completions (org-roam--get-ref-path-completions))
              (file (plist-get (cdr (assoc ref completions)) :path)))
    (find-file file)))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam--file-path-from-id (id)
  "The file path for an Org-roam file, with identifier ID."
  (file-truename
   (let* ((ext (or (car org-roam-file-extensions)
                   "org"))
          (file (concat id "." ext)))
     (expand-file-name
      (if org-roam-encrypt-files
          (concat file ".gpg")
        file)
      org-roam-directory))))

;;; The org-roam buffer
;;;; org-roam-link-face
(defface org-roam-link
  '((t :inherit org-link))
  "Face for Org-roam links."
  :group 'org-roam-faces)

(defface org-roam-link-current
  '((t :inherit org-link))
  "Face for Org-roam links pointing to the current buffer."
  :group 'org-roam-faces)

(defface org-roam-link-invalid
  '((t :inherit (error org-link)))
  "Face for Org-roam links that are not valid.
This face is used for links without a destination."
  :group 'org-roam-faces)

;;;; org-roam-backlinks-mode
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
                'org-roam-open-at-point nil 'local)
    (remove-hook 'org-open-at-point-functions
                 'org-roam-open-at-point 'local)))

(defun org-roam--in-buffer-p ()
  "Return t if in the Org-roam buffer."
  (and (boundp org-roam-backlinks-mode)
       org-roam-backlinks-mode))

(defun org-roam--retrieve-link-path (&optional pom)
  "Retrieve the path of the link at POM.
The point-or-marker POM can either be a position in the current
buffer or a marker."
  (let ((pom (or pom (point))))
    (org-with-point-at pom
      (plist-get (cadr (org-element-context)) :path))))

(defun org-roam--backlink-to-current-p ()
  "Return t if backlink is to the current Org-roam file."
  (let ((current (buffer-file-name org-roam-buffer--current))
        (backlink-dest (org-roam--retrieve-link-path)))
    (string= current backlink-dest)))

(defun org-roam--roam-link-face (path)
  "Conditional face for org file links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (cond ((not (file-exists-p path))
         'org-roam-link-invalid)
        ((and (org-roam--in-buffer-p)
              (org-roam--backlink-to-current-p))
         'org-roam-link-current)
        ((org-roam--org-roam-file-p path)
         'org-roam-link)
        (t
         'org-link)))

(defun org-roam-open-at-point ()
  "Open an Org-roam link or visit the text previewed at point.
When point is on an Org-roam link, open the link in the Org-roam window.
When point is on the Org-roam preview text, open the link in the Org-roam
window, and navigate to the point.
This function hooks into `org-open-at-point' via `org-open-at-point-functions'."
  (cond
   ;; Org-roam link
   ((let* ((context (org-element-context))
           (type (org-element-property :type context))
           (path (org-element-property :path context)))
      (when (and (eq (org-element-type context) 'link)
                 (string= "file" type)
                 (org-roam--org-roam-file-p (file-truename path)))
        (org-roam--find-file path)
        (org-show-context)
        t)))
   ;; Org-roam preview text
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (org-roam--find-file file-from)
      (goto-char p)
      (org-show-context)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(defun org-roam--find-file (file)
  "Open FILE in the window `org-roam' was called from."
  (if (and org-roam-last-window (window-valid-p org-roam-last-window))
      (progn (with-selected-window org-roam-last-window
               (find-file file))
             (select-window org-roam-last-window))
    (find-file file)))

(defun org-roam--get-backlinks (target)
  "Return the backlinks for TARGET.
TARGET may be a file, for Org-roam file links, or a citation key,
for Org-ref cite links."
  (org-roam-db-query [:select [from, to, properties] :from links
                      :where (= to $s1)
                      :order-by (asc from)]
                     target))

(defun org-roam-store-link ()
  "Store a link to an `org-roam' file."
  (when (org-before-first-heading-p)
    (when-let ((title (cdr (assoc "TITLE" (org-roam--extract-global-props '("TITLE"))))))
      (org-link-store-props
       :type        "file"
       :link        (format "file:%s" (abbreviate-file-name buffer-file-name))
       :description title))))

;;; The global minor org-roam-mode
(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when mode symbol `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam-buffer--update-maybe nil t)
    (add-hook 'after-save-hook #'org-roam-db--update-file nil t)
    (org-link-set-parameters "file" :face 'org-roam--roam-link-face :store #'org-roam-store-link)
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
                                (when (and (equal "file" type)
                                           (string-equal (file-truename path)
                                                         old-path))
                                  (set-marker (make-marker) (org-element-property :begin l))))))))
        (dolist (m link-markers)
          (goto-char m)
          (save-match-data
            (unless (org-in-regexp org-link-bracket-re 1)
              (user-error "No link at point"))
            (let* ((label (if (match-end 2)
                              (match-string-no-properties 2)
                            (org-link-unescape (match-string-no-properties 1))))
                   (new-label (if (string-equal label old-desc)
                                  new-desc
                                label)))
              (replace-match (org-link-make-string
                              (concat "file:" (file-relative-name new-path (file-name-directory (buffer-file-name))))
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
                      (when (and (equal "file" type)
                                 (f-relative-p path))
                        (cons (set-marker (make-marker)
                                          (org-element-property :begin link))
                              path)))))))
    (save-excursion
      (save-match-data
        (dolist (link links)
          (pcase-let ((`(,marker . ,path) link))
            (goto-char marker)
            (unless (org-in-regexp org-link-bracket-re 1)
              (user-error "No link at point"))
            (let* ((file-path (expand-file-name path (file-name-directory old-path)))
                   (new-path (file-relative-name file-path (file-name-directory (buffer-file-name)))))
              (replace-match (concat "file:" new-path)
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
               (org-roam--org-roam-file-p new-file))
      (org-roam-db--ensure-built)
      (let* ((old-path (file-truename old-file))
             (new-path (file-truename new-file))
             (old-slug (org-roam--get-title-or-slug old-file))
             (old-desc (org-roam--format-link-title old-slug))
             (new-slug (or (car (org-roam-db--get-titles old-path))
                           (org-roam--path-to-slug new-path)))
             (new-desc (org-roam--format-link-title new-slug))
             (files-to-rename (org-roam-db-query [:select :distinct [from]
                                                  :from links
                                                  :where (= to $s1)
                                                  :and (= type $s2)]
                                                 old-path
                                                 "roam")))
        ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
        (mapc (lambda (file)
                (setq file (if (string-equal (file-truename (car file)) old-path)
                               new-path
                             (car file)))
                (org-roam--replace-link file old-path new-path old-desc new-desc)
                (org-roam-db--update-file file))
              files-to-rename)
        ;; Remove database entries for old-file.org
        (org-roam-db--clear-file old-file)
        ;; If the new path is in a different directory, relative links
        ;; will break. Fix all file-relative links:
        (unless (string= (file-name-directory old-path)
                         (file-name-directory new-path))
          (with-current-buffer (or (find-buffer-visiting new-path)
                                   (find-file-noselect new-path))
            (org-roam--fix-relative-links old-path)
            (save-buffer)))
        (org-roam-db--update-file new-path)))))

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
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice)
    (org-roam-db-build-cache))
   (t
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    (org-roam-db--close-all)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (org-link-set-parameters "file" :face 'org-link)
        (remove-hook 'post-command-hook #'org-roam-buffer--update-maybe t)
        (remove-hook 'after-save-hook #'org-roam-db--update-file t))))))

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
(defun org-roam-find-file (&optional initial-prompt completions filter-fn)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details."
  (interactive)
  (when (org-roam-capture--in-process-p) (user-error "Org-roam capture in process"))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (org-roam-completion--completing-read "File: " completions
                                                                :initial-input initial-prompt))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (find-file file-path)
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(org-roam--title-to-slug title-with-tags))))
            (org-roam-capture--context 'title))
        (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
        (org-roam--with-template-error 'org-roam-capture-templates
          (org-roam-capture--capture))))))

;;;###autoload
(defun org-roam-find-directory ()
  "Find and open `org-roam-directory'."
  (interactive)
  (find-file org-roam-directory))

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
  (let* ((completions (org-roam--get-ref-path-completions arg filter))
         (ref (org-roam-completion--completing-read "Ref: "
                                                    completions
                                                    :require-match t))
         (file (-> (cdr (assoc ref completions))
                   (plist-get :path))))
    (find-file file)))

;;;###autoload
(defun org-roam-insert (&optional lowercase completions filter-fn description)
  "Find an Org-roam file, and insert a relative org link to it at point.
If LOWERCASE, downcase the title before insertion.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details."
  (interactive "P")
  (let* ((region (and (region-active-p)
                      ;; following may lose active region, so save it
                      (cons (region-beginning) (region-end))))
         (region-text (when region
                        (buffer-substring-no-properties (car region) (cdr region))))
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
                                                          description))))
    (if (and target-file-path
             (file-exists-p target-file-path))
        (progn
          (when region ;; Remove previously selected text.
            (delete-region (car region) (cdr region)))
          (insert (org-roam--format-link target-file-path link-description)))
      (when (org-roam-capture--in-process-p)
        (user-error "Nested Org-roam capture processes not supported"))
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug . ,(org-roam--title-to-slug title-with-tags))))
            (org-roam-capture--context 'title))
        (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--insert-link-h)
        (setq org-roam-capture-additional-template-props (list :region region
                                                               :link-description link-description
                                                               :capture-fn 'org-roam-insert))
        (org-roam--with-template-error 'org-roam-capture-templates
          (org-roam-capture--capture))))))

;;;###autoload
(defun org-roam-jump-to-index ()
  "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one."
  (interactive)
  (let ((index (org-roam--get-index-path)))
    (if (and index
             (file-exists-p index))
        (find-file index)
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
