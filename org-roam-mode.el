;;; org-roam-mode.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
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
;; This library implements the abstract major-mode `org-roam-mode', from which
;; almost all other Org-roam major-modes derive.
;;
;;; Code:
(require 'magit-section)

(require 'org-roam-utils)

(defvar org-roam-directory)
(defvar org-roam-find-file-hook)

(declare-function org-roam--org-file-p "org-roam")
(declare-function org-roam-node-at-point "org-roam-node")

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
(defvar org-roam-mode-sections nil
  "List of functions that insert sections for Org-roam.")

;;; The mode
(defvar org-roam-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'org-roam-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-visit-thing)
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

(defun org-roam-buffer ()
  "Launch an Org-roam buffer for the current node at point."
  (interactive)
  (if-let ((node (org-roam-node-at-point)))
      (progn
        (let ((buffer (get-buffer-create
                       (concat "org-roam: "
                               (file-relative-name (buffer-file-name) org-roam-directory)))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (org-roam-mode)
              (org-roam-set-header-line-format (org-roam-node-title node))
              (magit-insert-section (org-roam)
                (magit-insert-heading)
                (dolist (fn org-roam-mode-sections)
                  (funcall fn node)))))
          (switch-to-buffer-other-window buffer)))
    (user-error "No node at point")))

;;; Persistent buffer
(defvar org-roam-current-node nil
  "The current node at point.")

(defvar org-roam-buffer "*org-roam*"
  "The persistent Org-roam buffer name.")

(defun org-roam-buffer--post-command-h ()
  "Reconstructs `org-roam-buffer'.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
`org-roam-buffer'."
  (when (get-buffer-window org-roam-buffer)
    (when-let ((node (org-roam-node-at-point)))
      (unless (equal node org-roam-current-node)
        (setq org-roam-current-node node)
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
  "Toggle display of the `org-roam-buffer'."
  (interactive)
  (pcase (org-roam-buffer--visibility)
    ('visible
     (progn
       (delete-window (get-buffer-window org-roam-buffer))
       (remove-hook 'post-command-hook #'org-roam-buffer--post-command-h)))
    ((or 'exists 'none)
     (progn
       (display-buffer (get-buffer-create org-roam-buffer))
       (setq org-roam-current-node (org-roam-node-at-point))
       (org-roam-buffer-persistent-redisplay)))))

(defun org-roam-buffer-persistent-redisplay ()
  "Recompute contents of the persistent Org-roam buffer.
Has no effect when `org-roam-current-node' is nil."
  (when org-roam-current-node
    (with-current-buffer (get-buffer-create org-roam-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-mode)
        (org-roam-set-header-line-format (org-roam-node-title org-roam-current-node))
        (magit-insert-section (org-roam)
          (magit-insert-heading)
          (dolist (fn org-roam-mode-sections)
            (funcall fn org-roam-current-node)))))))

(defun org-roam-buffer--redisplay ()
  "."
  (add-hook 'post-command-hook #'org-roam-buffer--post-command-h nil t))

(add-hook 'org-roam-find-file-hook #'org-roam-buffer--redisplay)

(provide 'org-roam-mode)
;;; org-roam-mode.el ends here
