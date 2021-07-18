;;; org-roam-mode.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

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
;; This library implements the abstract major-mode `org-roam-mode', from which
;; almost all other Org-roam major-modes derive.
;;
;;; Code:
(require 'magit-section)

(require 'org-roam-utils)

(defvar org-roam-directory)
(defvar org-roam-find-file-hook)

(declare-function org-roam-node-at-point "org-roam")

;;; Faces
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

;;; Variables
(defvar org-roam-current-node nil
  "The current node at point.")

(defvar org-roam-current-directory nil
  "The `org-roam-directory' value for the current node.")

(defcustom org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                                 #'org-roam-reflinks-section)
  "Functions which insert sections of the `org-roam-buffer'.
Each function is called with one argument, which is the current org-roam node at point."
  :group 'org-roam
  :type 'hook)

;;; The mode
(defvar org-roam-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'org-roam-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-visit-thing)
    (define-key map [remap revert-buffer] 'org-roam-buffer-render)
    map)
  "Parent keymap for all keymaps of modes derived from `org-roam-mode'.")

(define-derived-mode org-roam-mode magit-section-mode "Org-roam"
  "Major mode for Org-roam's buffer."
  :group 'org-roam
  (face-remap-add-relative 'header-line 'org-roam-header-line))

;;; Key functions
(defun org-roam-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(defun org-roam-buffer-render ()
  "Render the current node at point."
  (interactive)
  (when (derived-mode-p 'org-roam-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local default-directory org-roam-current-directory)
      (setq-local org-roam-directory org-roam-current-directory)
      (org-roam-set-header-line-format (org-roam-node-title org-roam-current-node))
      (magit-insert-section (org-roam)
        (magit-insert-heading)
        (run-hook-with-args 'org-roam-mode-section-functions org-roam-current-node)))))

(defun org-roam-buffer ()
  "Launch an Org-roam buffer for the current node at point."
  (interactive)
  (if-let ((node (org-roam-node-at-point))
           (source-org-roam-directory org-roam-directory))
      (progn
        (let ((buffer (get-buffer-create
                       (concat "org-roam: "
                               (file-relative-name (buffer-file-name) org-roam-directory)))))
          (with-current-buffer buffer
            (org-roam-mode)
            (setq-local org-roam-current-node node)
            (setq-local org-roam-current-directory source-org-roam-directory)
            (org-roam-buffer-render))
          (switch-to-buffer-other-window buffer)))
    (user-error "No node at point")))

;;; Persistent buffer
(defvar org-roam-buffer "*org-roam*"
  "The persistent Org-roam buffer name.")

(defun org-roam-buffer--post-command-h ()
  "Reconstructs the Org-roam buffer.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
the Org-roam buffer."
  (when (get-buffer-window org-roam-buffer)
    (when-let ((node (org-roam-node-at-point)))
      (unless (equal node org-roam-current-node)
        (setq org-roam-current-node node)
        (setq org-roam-current-directory org-roam-directory)
        (org-roam-buffer-persistent-redisplay)))))

(define-inline org-roam-buffer--visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam-buffer-toggle ()
  "Toggle display of the Org-roam buffer."
  (interactive)
  (pcase (org-roam-buffer--visibility)
    ('visible
     (progn
       (delete-window (get-buffer-window org-roam-buffer))
       (remove-hook 'post-command-hook #'org-roam-buffer--post-command-h)))
    ((or 'exists 'none)
     (progn
       (setq org-roam-current-node (org-roam-node-at-point)
             org-roam-current-directory org-roam-directory)
       (display-buffer (get-buffer-create org-roam-buffer))
       (org-roam-buffer-persistent-redisplay)))))

(defun org-roam-buffer-persistent-redisplay ()
  "Recompute contents of the persistent Org-roam buffer.
Has no effect when `org-roam-current-node' is nil."
  (when org-roam-current-node
    (with-current-buffer (get-buffer-create org-roam-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-mode)
        (setq-local default-directory org-roam-current-directory)
        (setq-local org-roam-directory org-roam-current-directory)
        (org-roam-set-header-line-format (org-roam-node-title org-roam-current-node))
        (magit-insert-section (org-roam)
          (magit-insert-heading)
          (dolist (fn org-roam-mode-section-functions)
            (funcall fn org-roam-current-node)))))))

(defun org-roam-buffer--redisplay ()
  "."
  (add-hook 'post-command-hook #'org-roam-buffer--post-command-h nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-buffer--redisplay)

;;; Sections
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

;;;; Unlinked references
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

(provide 'org-roam-mode)
;;; org-roam-mode.el ends here
