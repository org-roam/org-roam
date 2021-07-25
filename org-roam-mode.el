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

;;; The mode
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

;;; Key functions
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

;;; Dedicated buffer
;;;###autoload (autoload 'org-roam-buffer-display-dedicated "org-roam" nil t)
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

;;; Persistent buffer
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
