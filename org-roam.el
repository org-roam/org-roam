;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (async "1.9.4") (org "9.0"))

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

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'org-element)
(require 'async)
(require 'subr-x)
(require 's)
(require 'f)
(require 'org-roam-utils)
(require 'eieio)

;;; Customizations
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/jethrokuan/org-roam")
  :link '(url-link :tag "Online Manual" "https://org-roam.readthedocs.io/"))

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Path to Org-roam files.

All Org files, at any level of nesting, is considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-new-file-directory nil
  "Path to where new Org-roam files are created.

If nil, default to the org-roam-directory (preferred)."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-mute-cache-build nil
  "Whether to mute the cache build message."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-buffer-position 'right
  "Position of `org-roam' buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'org-roam)

(defcustom org-roam-file-name-function #'org-roam--file-name-timestamp-title
  "The function used to generate filenames.

The function takes as parameter `TITLE', a string the user inputs."
  :group 'org-roam
  :type '(choice (const :tag "Default" org-roam--file-name-timestamp-title)
                 (function :tag "Personalized function")))

(defcustom org-roam-link-title-format "%s"
  "The format string used when inserting org-roam links that use their title."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-filename-noconfirm t
  "Whether to prompt for confirmation of fil name for new files.

If nil, always ask for filename."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-buffer-width 0.33 "Width of `org-roam' buffer."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer "*org-roam*"
  "Org-roam buffer name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files. If true, create files with .org.gpg extension."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Path to executable for viewing SVG."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graphviz-executable (executable-find "dot")
  "Path to graphviz executable."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-max-title-length 100
  "Maximum length of titles in graphviz graph nodes"
  :type 'number
  :group 'org-roam)

(defcustom org-roam-graph-node-shape "ellipse"
  "Maximum length of titles in graphviz graph nodes"
  :type 'string
  :group 'org-roam)

(defgroup org-roam-faces nil
  "Faces used by Org-Roam."
  :group 'org-roam
  :group 'faces)

;;; Polyfills
;; These are for functions I use that are only available in newer Emacs

;; Introduced in Emacs 27.1
(unless (fboundp 'make-empty-file)
  (defun make-empty-file (filename &optional parents)
    "Create an empty file FILENAME.
Optional arg PARENTS, if non-nil then creates parent dirs as needed.

If called interactively, then PARENTS is non-nil."
    (interactive
     (let ((filename (read-file-name "Create empty file: ")))
       (list filename t)))
    (when (and (file-exists-p filename) (null parents))
      (signal 'file-already-exists `("File exists" ,filename)))
    (let ((paren-dir (file-name-directory filename)))
      (when (and paren-dir (not (file-exists-p paren-dir)))
        (make-directory paren-dir parents)))
    (write-region "" nil filename nil 0)))

;;; Classes

(defclass org-roam-cache ()
  ((initialized :initarg :initialized
                :documentation "Is cache valid?")
   (forward-links :initarg :forward-links
                  :documentation "Cache containing forward links.")
   (backward-links :initarg :backward-links
                   :documentation "Cache containing backward links.")
   (titles :initarg :titles
           :documentation "Cache containing titles for org-roam files."))
  "All cache for an org-roam directory.")

;;; Dynamic variables
(defvar org-roam--current-buffer nil
  "Currently displayed file in `org-roam' buffer.")

(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defvar org-roam--cache nil
  "The list of cache separated by directory.")

;;; Utilities
(defun org-roam-directory-normalized ()
  "Get the org-roam-directory normalized so that it can be used
as a unique key."
  (directory-file-name (file-truename org-roam-directory)))

(defmacro org-roam--get-local (name)
  "Get a variable that is local to the current org-roam-directory."
  `(alist-get (org-roam-directory-normalized) ,name nil nil #'equal))

(defmacro org-roam--set-local (name value)
  "Set a variable that is local to the current org-roam-directory."
  `(setf (alist-get (org-roam-directory-normalized) ,name nil nil #'equal)
         ,value))

(defun org-roam--get-directory-cache ()
  "Get the cache object for the current org-roam-directory."
  (let* ((cache (org-roam--get-local org-roam--cache)))
    (if cache
        cache
      (let ((new-cache (org-roam--default-cache)))
        (org-roam--set-local org-roam--cache new-cache)
        new-cache))))

(defun org-roam--set-directory-cache (data)
  "Set the cache object for the current org-roam-directory."
  (setf (alist-get (org-roam-directory-normalized)
                   org-roam--cache nil nil #'equal) data))

(defun org-roam-cache-initialized ()
  "Is cache valid?"
  (oref (org-roam--get-directory-cache) initialized))

(defun org-roam-forward-links-cache ()
  "Cache containing forward links."
  (oref (org-roam--get-directory-cache) forward-links))

(defun org-roam-backward-links-cache ()
  "Cache containing backward links."
  (oref (org-roam--get-directory-cache) backward-links))

(defun org-roam-titles-cache ()
  "Cache containing titles for org-roam files."
  (oref (org-roam--get-directory-cache) titles))

(defun org-roam--ensure-cache-built ()
  "Ensures that org-roam cache is built."
  (unless (org-roam-cache-initialized)
    (org-roam--build-cache-async)
    (user-error "Your Org-Roam cache isn't built yet! Please wait")))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of org-roam system, defaulting to the name of the current buffer. Else, return nil."
  (let ((path (or file
                  (buffer-file-name (current-buffer)))))
    (and path
         (org-roam--org-file-p path)
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory)))))

(defun org-roam--get-title-from-cache (file)
  "Return title of `FILE' from the cache."
  (or (gethash file (org-roam-titles-cache))
      (progn
        (unless (org-roam-cache-initialized)
          (user-error "The Org-Roam caches aren't built! Please run org-roam--build-cache-async"))
        nil)))

(defun org-roam--find-all-files ()
  "Return all org-roam files."
  (org-roam--find-files (file-truename org-roam-directory)))

(defun org-roam--new-file-path (id &optional absolute)
  "Make new file path from identifier `ID'.

If `ABSOLUTE', return an absolute file-path. Else, return a relative file-path."
  (let ((absolute-file-path (file-truename
                             (expand-file-name
                              (if org-roam-encrypt-files
                                  (concat id ".org.gpg")
                                (concat id ".org"))
                              (or org-roam-new-file-directory
                                  org-roam-directory)))))
    (if absolute
        absolute-file-path
      (file-relative-name absolute-file-path
                          (file-truename org-roam-directory)))))

(defun org-roam--get-title-or-slug (file-path)
  "Convert `FILE-PATH' to the file title, if it exists. Else, return the path."
  (or (org-roam--get-title-from-cache file-path)
      (-> file-path
          (file-relative-name (file-truename org-roam-directory))
          (file-name-sans-extension))))

(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet ((replace (title pair)
                     (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'replace title pairs)))
      (s-downcase slug))))

(defun org-roam--file-name-timestamp-title (title)
  "Return a file name (without extension) for new files.

It uses TITLE and the current timestamp to form a unique title."
  (let ((timestamp (format-time-string "%Y%m%d%H%M%S" (current-time)))
        (slug (org-roam--title-to-slug title)))
    (format "%s_%s" timestamp slug)))

;;; Creating org-roam files
(defvar org-roam-templates
  (list (list "default" (list :file #'org-roam--file-name-timestamp-title
                              :content "#+TITLE: ${title}")))
  "Templates to insert for new files in org-roam.")

(defun org-roam--make-new-file (title &optional template-key)
  (unless org-roam-templates
    (user-error "No templates defined"))
  (let (template)
    (if template-key
        (setq template (cadr (assoc template-key org-roam-templates)))
      (if (= (length org-roam-templates) 1)
          (setq template (cadar org-roam-templates))
        (setq template
              (cadr (assoc (completing-read "Template: " org-roam-templates)
                           org-roam-templates)))))
    (let (file-name-fn file-path)
      (fset 'file-name-fn (plist-get template :file))
      (setq file-path (org-roam--new-file-path (file-name-fn title) t))
      (if (file-exists-p file-path)
          file-path
        (make-empty-file file-path t)
        (write-region
         (s-format (plist-get template :content)
                   'aget
                   (list (cons "title" title)
                         (cons "slug" (org-roam--title-to-slug title))))
         nil file-path nil)
        file-path))))

(defun org-roam--get-new-id (title)
  "Return a new ID, given the note TITLE."
  (let* ((proposed-slug (funcall org-roam-file-name-function title))
         (new-slug (if org-roam-filename-noconfirm
                       proposed-slug
                     (read-string "Enter ID (without extension): "
                                  proposed-slug)))
         (file-path (org-roam--new-file-path new-slug t)))
    (if (file-exists-p file-path)
        (user-error "There's already a file at %s")
      new-slug)))

;;; Inserting org-roam links
(defun org-roam-insert (prefix)
  "Find an org-roam file, and insert a relative org link to it at point.

If PREFIX, downcase the title before insertion."
  (interactive "P")
  (let* ((region (and (region-active-p)
                      ;; following may lose active region, so save it
                      (cons (region-beginning) (region-end))))
         (region-text (when region
                        (buffer-substring-no-properties
                         (car region) (cdr region))))
         (completions (mapcar (lambda (file)
                                (list (org-roam--get-title-or-slug file)
                                      file))
                              (org-roam--find-all-files)))
         (title (completing-read "File: " completions nil nil region-text))
         (region-or-title (or region-text title))
         (absolute-file-path (or (cadr (assoc title completions))
                                 (org-roam--make-new-file title)))
         (current-file-path (-> (or (buffer-base-buffer)
                                    (current-buffer))
                                (buffer-file-name)
                                (file-truename)
                                (file-name-directory))))
    (when region ;; Remove previously selected text.
      (goto-char (car region))
      (delete-char (- (cdr region) (car region))))
    (insert (format "[[%s][%s]]"
                    (concat "file:" (file-relative-name absolute-file-path
                                                        current-file-path))
                    (format org-roam-link-title-format (if prefix
                                                           (downcase region-or-title)
                                                         region-or-title))))))

;;; Finding org-roam files
(defun org-roam-find-file ()
  "Find and open an org-roam file."
  (interactive)
  (let* ((completions (mapcar (lambda (file)
                                (list (org-roam--get-title-or-slug file) file))
                              (org-roam--find-all-files)))
         (title-or-slug (completing-read "File: " completions))
         (absolute-file-path (or (cadr (assoc title-or-slug completions))
                                 (org-roam--make-new-file title-or-slug))))
    (find-file absolute-file-path)))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam-switch-to-buffer ()
  "Switch to an existing org-roam buffer using completing-read."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (org-roam--get-title-from-cache
                                                 (buffer-file-name buffer))
                                                (buffer-name buffer))
                                            buffer))
                                    roam-buffers)))
    (unless roam-buffers
      (error "No roam buffers."))
    (when-let ((name (completing-read "Choose a buffer: " names-and-buffers)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

(defvar org-roam--ongoing-async-build nil
  "Prevent multiple async cache builds.  This can happen when
  restoring a session or loading multiple org-roam files before a
  build has completed.")

;;; Building the org-roam cache
(defun org-roam--build-cache-async (&optional on-success)
  "Builds the caches asychronously."
  (interactive)
  (let ((existing (org-roam--get-local org-roam--ongoing-async-build)))
    (unless (and (processp existing)
                 (not (async-ready existing)))
      (org-roam--set-local
       org-roam--ongoing-async-build
       (async-start
        `(lambda ()
           (setq load-path ',load-path)
           (package-initialize)
           (require 'org-roam-utils)
           ,(async-inject-variables "org-roam-directory")
           (org-roam--build-cache org-roam-directory))
        (lambda (cache)
          (let ((org-roam-directory (plist-get cache :directory)))
            (org-roam--set-directory-cache
             (org-roam-cache :initialized t
                             :forward-links (plist-get cache :forward)
                             :backward-links (plist-get cache :backward)
                             :titles (plist-get cache :titles)))
            (unless org-roam-mute-cache-build
              (message "Org-roam cache built!"))
            (when on-success
              (funcall on-success)))))))))

(defun org-roam--clear-cache ()
  "Clears all entries in the caches."
  (interactive)
  (org-roam--set-directory-cache (org-roam--default-cache)))

(defun org-roam--default-cache ()
  "A default, uninitialized cache object."
  (org-roam-cache :initialized nil
                  :forward-links (make-hash-table :test #'equal)
                  :backward-links (make-hash-table :test #'equal)
                  :titles (make-hash-table :test #'equal)))

(defun org-roam--clear-file-from-cache (&optional filepath)
  "Remove any related links to the file.

This is equivalent to removing the node from the graph."
  (let* ((path (or filepath
                   (buffer-file-name (current-buffer))))
         (file (file-truename path)))
    ;; Step 1: Remove all existing links for file
    (when-let ((forward-links (gethash file (org-roam-forward-links-cache))))
      ;; Delete backlinks to file
      (dolist (link forward-links)
        (when-let ((backward-links (gethash link (org-roam-backward-links-cache))))
          (remhash file backward-links)
          (puthash link backward-links (org-roam-backward-links-cache))))
      ;; Clean out forward links
      (remhash file (org-roam-forward-links-cache)))
    ;; Step 2: Remove from the title cache
    (remhash file (org-roam-titles-cache))))

(defun org-roam--update-cache-title ()
  "Insert the title of the current buffer into the cache."
  (when-let ((title (org-roam--extract-title)))
    (puthash (file-truename (buffer-file-name (current-buffer)))
             title
             (org-roam-titles-cache))))

(defun org-roam--update-cache ()
  "Update org-roam caches for the current buffer file."
  (save-excursion
    (org-roam--clear-file-from-cache)
    ;; Insert into title cache
    (org-roam--update-cache-title)
    ;; Insert new items
    (let ((items (org-roam--parse-content)))
      (dolist (item items)
        (org-roam--insert-item
         item
         :forward (org-roam-forward-links-cache)
         :backward (org-roam-backward-links-cache))))
    ;; Rerender buffer
    (org-roam--maybe-update-buffer :redisplay t)))

;;; Org-roam daily notes

(defun org-roam--file-for-time (time)
  "Create and find file for TIME."
  (let* ((org-roam-templates (list (list "daily" (list :file (lambda (title) title)
                                                       :content "#+TITLE: ${title}")))))
    (org-roam--make-new-file (format-time-string "%Y-%m-%d" time) "daily")))

(defun org-roam-today ()
  "Create and find file for today."
  (interactive)
  (let ((path (org-roam--file-for-time (current-time))))
    (find-file path)))

(defun org-roam-tomorrow ()
  "Create and find the file for tomorrow."
  (interactive)
  (let ((path (org-roam--file-for-time (time-add 86400 (current-time)))))
    (find-file path)))

(defun org-roam-date ()
  "Create the file for any date using the calendar."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (let ((path (org-roam--file-for-time time)))
      (find-file path))))

;;; Org-roam buffer
(define-derived-mode org-roam-backlinks-mode org-mode "Backlinks"
  "Major mode for the org-roam backlinks buffer

Bindings:
\\{org-roam-backlinks-mode-map}")

(define-key org-roam-backlinks-mode-map [mouse-1] 'org-roam-jump-to-backlink)
(define-key org-roam-backlinks-mode-map (kbd "RET") 'org-roam-jump-to-backlink)

(defun org-roam-jump-to-backlink ()
  "Jumps to original file and location of the backlink content snippet at point"
  (interactive)
  (let ((file-from (get-text-property (point) 'file-from))
        (p (get-text-property (point) 'file-from-point)))
    (when (and file-from p)
      (find-file file-from)
      (goto-char p)
      (org-show-context))))

(defun org-roam--find-file (file)
  "Open FILE in the window `org-roam' was called from."
  (if (and org-roam-last-window (window-valid-p org-roam-last-window))
      (progn (with-selected-window org-roam-last-window
               (find-file file))
             (select-window org-roam-last-window))
    (find-file file)))

(defun org-roam-update (file-path)
  "Show the backlinks for given org file for file at `FILE-PATH'."
  (let* ((source-org-roam-directory org-roam-directory))
    (org-roam--ensure-cache-built)
    (let ((buffer-title (org-roam--get-title-or-slug file-path)))
      (with-current-buffer org-roam-buffer
        ;; When dir-locals.el is used to override org-roam-directory,
        ;; org-roam-buffer may have a different local org-roam-directory.
        (let ((org-roam-directory source-org-roam-directory))
          ;; Locally overwrite the file opening function to re-use the
          ;; last window org-roam was called from
          (setq-local
           org-link-frame-setup
           (cons '(file . org-roam--find-file) org-link-frame-setup))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (when (not (eq major-mode 'org-roam-backlinks-mode))
              (org-roam-backlinks-mode))
            (make-local-variable 'org-return-follows-link)
            (setq org-return-follows-link t)
            (insert
             (propertize buffer-title 'font-lock-face 'org-document-title))
            (if-let ((backlinks (gethash file-path (org-roam-backward-links-cache))))
                (progn
                  (insert (format "\n\n* %d Backlinks\n"
                                  (hash-table-count backlinks)))
                  (maphash (lambda (file-from contents)
                             (insert (format "** [[file:%s][%s]]\n"
                                             file-from
                                             (org-roam--get-title-or-slug file-from)))
                             (dolist (properties contents)
                               (let ((content (propertize
                                               (s-trim (s-replace "\n" " "
                                                                  (plist-get properties :content)))
                                               'font-lock-face 'org-block
                                               'help-echo "mouse-1: visit backlinked note"
                                               'file-from file-from
                                               'file-from-point (plist-get properties :point))))
                                 (insert (format "%s \n\n" content)))))
                           backlinks))
              (insert "\n\n* No backlinks!")))
          (read-only-mode 1))))))

;;; Building the Graphviz graph
(defun org-roam-build-graph ()
  "Build graphviz graph output."
  (org-roam--ensure-cache-built)
  (with-temp-buffer
	(insert "digraph {\n")
	(dolist (file (org-roam--find-all-files))
	  (let ((title (org-roam--get-title-or-slug file)))
		(let ((shortened-title (s-truncate org-roam-graph-max-title-length title)))
		  (insert
		   (format "  \"%s\" [label=\"%s\", shape=%s, URL=\"roam://%s\", tooltip=\"%s\"];\n"
				   title
				   shortened-title
				   org-roam-graph-node-shape
				   file
				   title
				   )))))
	  (maphash
	   (lambda (from-link to-links)
		 (dolist (to-link to-links)
		   (insert (format "  \"%s\" -> \"%s\";\n"
						   (org-roam--get-title-or-slug from-link)
						   (org-roam--get-title-or-slug to-link)))))
	   (org-roam-forward-links-cache))
	  (insert "}")
	  (buffer-string)))

(defun org-roam-show-graph ()
  "Generate the org-roam graph in SVG format, and display it using `org-roam-graph-viewer'."
  (interactive)
  (unless org-roam-graphviz-executable
    (setq org-roam-graphviz-executable (executable-find "dot")))
  (unless org-roam-graphviz-executable
    (user-error "Can't find graphviz executable. Please check if it is in your path"))
  (declare (indent 0))
  (let ((temp-dot (expand-file-name "graph.dot" temporary-file-directory))
        (temp-graph (expand-file-name "graph.svg" temporary-file-directory))
        (graph (org-roam-build-graph)))
    (with-temp-file temp-dot
      (insert graph))
    (call-process org-roam-graphviz-executable nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (if (and org-roam-graph-viewer (executable-find org-roam-graph-viewer))
	(call-process org-roam-graph-viewer nil 0 nil temp-graph)
      (view-file temp-graph))))

;;; Org-roam minor mode
(cl-defun org-roam--maybe-update-buffer (&key redisplay)
  "Update `org-roam-buffer' with the necessary information.
This needs to be quick/infrequent, because this is run at
`post-command-hook'."
  (let ((buffer (window-buffer)))
    (when (and (or redisplay
                   (not (eq org-roam--current-buffer buffer)))
               (eq 'visible (org-roam--current-visibility))
               (org-roam-cache-initialized)
               (buffer-local-value 'buffer-file-truename buffer))
      (setq org-roam--current-buffer buffer)
      (org-roam-update (expand-file-name
                        (buffer-local-value 'buffer-file-truename buffer))))))

(defface org-roam-link
  '((t :inherit org-link))
  "Face for org-roam link."
  :group 'org-roam-faces)

(defun org-roam--roam-link-face (path)
  "Conditional face for org file links.

Applies `org-roam-link-face' if PATH correponds to a Roam file."
  (if (org-roam--org-roam-file-p path)
      'org-roam-link
    'org-link))

(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam--maybe-update-buffer nil t)
    (add-hook 'after-save-hook #'org-roam--update-cache nil t)
    (if (org-roam-cache-initialized)
        (org-roam--setup-found-file)
      (org-roam--build-cache-async
       (let ((buf (buffer-name)))
         #'(lambda ()
             (with-current-buffer buf
               (org-roam--setup-found-file))))))))

(defun org-roam--setup-found-file ()
  "Setup a buffer recognized via the \"find-file-hook\"."
  (org-link-set-parameters "file" :face 'org-roam--roam-link-face)
  (org-roam--maybe-update-buffer :redisplay nil))

(defvar org-roam-mode-map
  (make-sparse-keymap)
  "Keymap for org-roam commands.")

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency during file deletes."
  (org-roam--clear-file-from-cache (file-truename file)))

(defun org-roam--rename-file-advice (file new-file &rest args)
  "Rename backlinks of FILE to refer to NEW-FILE."
  (when (and (not (auto-save-file-name-p file))
             (not (auto-save-file-name-p new-file))
             (org-roam--org-roam-file-p new-file))
    (org-roam--ensure-cache-built)
    (org-roam--clear-file-from-cache file)

    (let* ((files (gethash file (org-roam-backward-links-cache) nil))
           (path (file-truename file))
           (new-path (file-truename new-file))
           (slug (org-roam--get-title-or-slug file))
           (old-title (format org-roam-link-title-format slug))
           (new-slug (or (org-roam--get-title-from-cache path)
                         (org-roam--get-title-or-slug new-path)))
           (new-title (format org-roam-link-title-format new-slug)))
      (when files
        (maphash (lambda (file-from props)
                   (let* ((file-dir (file-name-directory file-from))
                          (relative-path (file-relative-name new-path file-dir))
                          (old-relative-path (file-relative-name path file-dir))
                          (slug-regex (regexp-quote (format "[[file:%s][%s]]" old-relative-path old-title)))
                          (named-regex (concat
                                        (regexp-quote (format "[[file:%s][" old-relative-path))
                                        "\\(.*\\)"
                                        (regexp-quote "]]"))))
                     (with-temp-file file-from
                       (insert-file-contents file-from)
                       (while (re-search-forward slug-regex  nil t)
                         (replace-match (format "[[file:%s][%s]]" relative-path new-title)))
                       (goto-char (point-min))
                       (while (re-search-forward named-regex nil t)
                         (replace-match (format "[[file:%s][\\1]]" relative-path))))
                     (save-window-excursion
                       (find-file file-from)
                       (org-roam--update-cache))))
                 files))
      (save-window-excursion
        (find-file new-path)
        (org-roam--update-cache)))))

;;;###autoload
(define-minor-mode org-roam-mode
  "Minor mode for Org-roam.

When called interactively, toggle `org-roam-mode'. with prefix ARG, enable `org-roam-mode'
if ARG is posiwive, otherwise disable it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted, nil, or positive.
If ARG is `toggle', toggle `org-roam-mode'. Otherwise, behave as if called interactively."
  :lighter " Org-Roam"
  :keymap  org-roam-mode-map
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (unless (org-roam-cache-initialized)
      (org-roam--build-cache-async))
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice))
   (t
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (org-link-set-parameters "file" :face 'org-link)
        (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer t)
        (remove-hook 'after-save-hook #'org-roam--update-cache t))))))

(provide 'org-roam)

;;; Show/hide the org-roam buffer
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
  "Set the width of the org-roam buffer to `WIDTH'."
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
;;; org-roam.el ends here

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
