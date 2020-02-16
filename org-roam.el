;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (async "1.9.4"))

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
  :type 'directoy
  :group 'org-roam)

(defcustom org-roam-buffer-position 'right
  "Position of `org-roam' buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'org-roam)

(defcustom org-roam-file-format "%Y%m%d%H%M%S"
  "The timestamp format to use filenames."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The format string used when inserting org-roam links that use their title."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-use-timestamp-as-filename t
  "Whether to use timestamp as a file name. If not true, prompt for a file name each time."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-autopopulate-title t "Whether to autopopulate the title."
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

;;; Dynamic variables
(defvar org-roam-cache-initialized nil
  "Boolean value indicating whether the cache is initialized.")

(defvar org-roam-forward-links-cache (make-hash-table :test #'equal)
  "Cache containing forward links.")

(defvar org-roam-backward-links-cache (make-hash-table :test #'equal)
  "Cache containing backward-links.")

(defvar org-roam-titles-cache (make-hash-table :test #'equal)
  "Cache containing titles for org-roam files.")

(defvar org-roam-current-file nil
  "Currently displayed file in `org-roam' buffer.")

;;; Utilities
(defun org-roam--ensure-cache-built ()
  "Ensures that org-roam cache is built."
  (unless org-roam-cache-initialized
    (org-roam--build-cache-async)
    (user-error "Your Org-Roam cache isn't built yet! Please wait")))

(defun org-roam--org-roam-file-p ()
  "Return t if file is part of org-roam system, false otherwise."
  (and (buffer-file-name (current-buffer))
       (f-descendant-of-p (file-truename (buffer-file-name (current-buffer)))
                          (file-truename org-roam-directory))))

(defun org-roam--get-title-from-cache (file)
  "Return title of `FILE' from the cache."
  (or (gethash file org-roam-titles-cache)
      (progn
        (unless org-roam-cache-initialized
          (user-error "The Org-Roam caches aren't built! Please run org-roam--build-cache-async"))
        nil)))

(defun org-roam--find-all-files ()
  "Return all org-roam files."
  (org-roam--find-files (file-truename org-roam-directory)))

(defun org-roam--make-new-file-path (id &optional absolute)
  "Make new file path from identifier `ID'.

If `ABSOLUTE', return an absolute file-path. Else, return a relative file-path."
  (let ((absolute-file-path (file-truename
                             (expand-file-name
                              (if org-roam-encrypt-files
                                  (concat id ".org.gpg")
                                (concat id ".org"))
                              org-roam-directory))))
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
  (let* ((s (s-downcase title))
         (s (replace-regexp-in-string "[^a-zA-Z0-9_ ]" "" s))
         (s (s-split " " s))
         (s (s-join "_" s)))
    s))


;;; Creating org-roam files
(defun org-roam--populate-title (file &optional title)
  "Populate title line for FILE using TITLE, if provided.
If not provided, derive the title from the file name."
  (let ((title (or title
                   (-> file
                       (file-name-nondirectory)
                       (file-name-sans-extension)
                       (split-string "_")
                       (string-join " ")
                       (s-titleize)))))
    (write-region
     (concat
      "#+TITLE: "
      title
      "\n\n")
     nil file nil)))

(defun org-roam--make-file (file-path &optional title)
  "Create an org-roam file at FILE-PATH, optionally setting the TITLE attribute."
  (if (file-exists-p file-path)
      (error (format "Aborting, file already exists at %s" file-path))
    (if org-roam-autopopulate-title
        (org-roam--populate-title file-path title)
      (make-empty-file file-path))))

(defun org-roam--new-file-named (slug)
  "Create a new file named `SLUG'.
`SLUG' is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (let ((file-path (org-roam--make-new-file-path slug t)))
    (unless (file-exists-p file-path)
      (org-roam--make-file file-path))
    (find-file file-path)))

(defun org-roam--get-new-id (&optional title)
  "Return a new ID, generated from the current time.

Optionally pass it the title, for a smart file name."
  (if org-roam-use-timestamp-as-filename
      (format-time-string org-roam-file-format (current-time))
    (let* ((slug (read-string "Enter ID (without extension): "
                              (if title
                                  (org-roam--title-to-slug title)
                                "")))
           (file-path (org-roam--make-new-file-path slug t)))
      (if (file-exists-p file-path)
          (user-error "There's already a file at %s")
        slug))))

(defun org-roam-new-file ()
  "Quickly create a new file, using the current timestamp."
  (interactive)
  (org-roam--new-file-named (org-roam--get-new-id)))

;;; Inserting org-roam links
(defun org-roam-insert ()
  "Find an org-roam file, and insert a relative org link to it at point."
  (interactive)
  (let* ((completions (mapcar (lambda (file)
                                (list (org-roam--get-title-or-slug file)
                                      file))
                              (org-roam--find-all-files)))
         (title (completing-read "File: " completions))
         (absolute-file-path (or (cadr (assoc title completions))
                                 (org-roam--make-new-file-path (org-roam--get-new-id title) t)))
         (current-file-path (-> (current-buffer)
                                (buffer-file-name)
                                (file-truename)
                                (file-name-directory))))
    (unless (file-exists-p absolute-file-path)
      (org-roam--make-file absolute-file-path title))
    (insert (format "[[%s][%s]]"
                    (concat "file:" (file-relative-name absolute-file-path
                                                        current-file-path))
                    (format org-roam-link-title-format title)))))

;;; Finding org-roam files
(defun org-roam-find-file ()
  "Find and open an org-roam file."
  (interactive)
  (let* ((completions (mapcar (lambda (file)
                                (list (org-roam--get-title-or-slug file) file))
                              (org-roam--find-all-files)))
         (title-or-slug (completing-read "File: " completions))
         (absolute-file-path (or (cadr (assoc title-or-slug completions))
                                 (org-roam--make-new-file-path
                                  (org-roam--get-new-id title-or-slug) t))))
    (unless (file-exists-p absolute-file-path)
      (org-roam--make-file absolute-file-path title-or-slug))
    (find-file absolute-file-path)))

;;; Building the org-roam cache (asynchronously)
(defun org-roam--build-cache-async ()
  "Builds the cache asychronously, saving it into the org-roam caches."
  (interactive)
  (async-start
   `(lambda ()
      (setq load-path ',load-path)
      (package-initialize)
      (require 'org-roam-utils)
      ,(async-inject-variables "org-roam-directory")
      (let ((backward-links (make-hash-table :test #'equal))
            (forward-links (make-hash-table :test #'equal))
            (file-titles (make-hash-table :test #'equal)))
        (let* ((org-roam-files (org-roam--find-files org-roam-directory))
               (file-items (mapcar (lambda (file)
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (org-roam--parse-content file))) org-roam-files)))
          (dolist (items file-items)
            (dolist (item items)
              (org-roam--insert-item
               item
               :forward forward-links
               :backward backward-links)))
          (mapcar (lambda (file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (when-let ((title (org-roam--extract-title)))
                        (puthash file title file-titles))))
                  org-roam-files))
        (list
         :forward forward-links
         :backward backward-links
         :titles file-titles)))
   (lambda (cache)
     (setq org-roam-forward-links-cache (plist-get cache :forward))
     (setq org-roam-backward-links-cache (plist-get cache :backward))
     (setq org-roam-titles-cache (plist-get cache :titles))
     (setq org-roam-cache-initialized t)
     (message "Org-roam cache built!"))))

(defun org-roam--clear-cache ()
  "Remove any related links to the file.

This is equivalent to removing the node from the graph."
  (let ((file (file-truename (buffer-file-name (current-buffer)))))
    ;; Step 1: Remove all existing links for file
    (when-let ((forward-links (gethash file org-roam-forward-links-cache)))
      ;; Delete backlinks to file
      (dolist (link forward-links)
        (when-let ((backward-links (gethash link org-roam-backward-links-cache)))
          (remhash file backward-links)
          (puthash link backward-links org-roam-backward-links-cache)))
      ;; Clean out forward links
      (remhash file org-roam-forward-links-cache))
    ;; Step 2: Remove from the title cache
    (remhash file org-roam-titles-cache)))

(defun org-roam--update-cache-title ()
  "Insert the title of the current buffer into the cache."
  (when-let ((title (org-roam--extract-title)))
    (puthash (file-truename (buffer-file-name (current-buffer)))
             title
             org-roam-titles-cache)))

(defun org-roam--update-cache ()
  "Update org-roam caches for the current buffer file."
  (save-excursion
    (org-roam--clear-cache)
    ;; Insert into title cache
    (org-roam--update-cache-title)
    ;; Insert new items
    (let ((items (org-roam--parse-content)))
      (dolist (item items)
        (org-roam--insert-item
         item
         :forward org-roam-forward-links-cache
         :backward org-roam-backward-links-cache)))
    ;; Rerender buffer
    (org-roam--maybe-update-buffer :redisplay t)))

;;; Org-roam daily notes
(defun org-roam-today ()
  "Create the file for today."
  (interactive)
  (org-roam--new-file-named (format-time-string "%Y-%m-%d" (current-time))))

(defun org-roam-tomorrow ()
  "Create the file for tomorrow."
  (interactive)
  (org-roam--new-file-named (format-time-string "%Y-%m-%d" (time-add 86400 (current-time)))))

(defun org-roam-date ()
  "Create the file for any date using the calendar."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (org-roam--new-file-named (format-time-string "%Y-%m-%d" time))))

;;; Org-roam buffer updates
(defun org-roam-update (file-path)
  "Show the backlinks for given org file for file at `FILE-PATH'."
  (org-roam--ensure-cache-built)
  (let ((buffer-title (org-roam--get-title-or-slug file-path)))
    (with-current-buffer org-roam-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (not (eq major-mode 'org-mode))
          (org-mode))
        (make-local-variable 'org-return-follows-link)
        (setq org-return-follows-link t)
        (insert
         (propertize buffer-title 'font-lock-face 'org-document-title))
        (if-let ((backlinks (gethash file-path org-roam-backward-links-cache)))
            (progn
              (insert (format "\n\n* %d Backlinks\n"
                              (hash-table-count backlinks)))
              (maphash (lambda (file-from contents)
                         (insert (format "** [[file:%s][%s]]\n"
                                         file-from
                                         (org-roam--get-title-or-slug file-from)))
                         (dolist (content contents)
                           (insert (concat (propertize (s-trim (s-replace "\n" " " content))
                                                       'font-lock-face 'org-block)
                                           "\n\n"))))
                       backlinks))
          (insert "\n\n* No backlinks!")))
      (read-only-mode 1)))
  (setq org-roam-current-file file-path))

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
  (pcase (org-roam--current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam--setup-buffer))
    ('none (org-roam--setup-buffer))))

;;; The minor mode definition that updates the buffer
(defun org-roam--maybe-enable ()
  "Enable org-roam updating for file, if file is an org-roam file."
  (when (org-roam--org-roam-file-p)
    (org-roam--enable)))

(defun org-roam--enable ()
  "Enable org-roam updating for file.

1. If the cache does not yet exist, build it asynchronously.
2. Setup hooks for updating the cache, and the org-roam buffer."
  (unless org-roam-cache-initialized
    (org-roam--build-cache-async))
  (add-hook 'post-command-hook #'org-roam--maybe-update-buffer nil t)
  (add-hook 'after-save-hook #'org-roam--update-cache nil t))

(defun org-roam--disable ()
  "Disable org-roam updating for file.

1. Remove hooks for updating the cache, and the org-roam buffer."
  (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer)
  (remove-hook 'after-save-hook #'org-roam--update-cache))

(cl-defun org-roam--maybe-update-buffer (&key redisplay)
  "Update `org-roam-buffer' with the necessary information.
This needs to be quick/infrequent, because this is run at
`post-command-hook'."
  (with-current-buffer (window-buffer)
    (when (and (get-buffer org-roam-buffer)
               (buffer-file-name (current-buffer))
               (file-exists-p (file-truename (buffer-file-name (current-buffer))))
               (or redisplay
                   (not (string= org-roam-current-file
                                 (file-truename (buffer-file-name (current-buffer)))))))
      (org-roam-update (file-truename (buffer-file-name (window-buffer)))))))

(define-minor-mode org-roam-mode
  "Global minor mode to automatically update the org-roam buffer."
  :require 'org-roam
  (if org-roam-mode
      (org-roam--maybe-enable)
    (org-roam--disable)))

;;; Building the Graphviz graph
(defun org-roam-build-graph ()
  "Build graphviz graph output."
  (org-roam--ensure-cache-built)
  (with-temp-buffer
    (insert "digraph {\n")
    (dolist (file (org-roam--find-all-files))
      (insert
       (format "  \"%s\" [URL=\"roam://%s\"];\n"
               (org-roam--get-title-or-slug file)
               file)))
    (maphash
     (lambda (from-link to-links)
       (dolist (to-link to-links)
         (insert (format "  \"%s\" -> \"%s\";\n"
                         (org-roam--get-title-or-slug from-link)
                         (org-roam--get-title-or-slug to-link)))))
     org-roam-forward-links-cache)
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
    (call-process org-roam-graph-viewer nil 0 nil temp-graph)))

(provide 'org-roam)

;;; org-roam.el ends here

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
