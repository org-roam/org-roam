;;; org-roam-buffer.el --- Metadata buffer -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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
;; This library provides the org-roam-buffer functionality for org-roam
;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ol)
(require 'org-element)
(require 'org-roam-macs)

(defvar org-roam-directory)
(defvar org-link-frame-setup)
(defvar org-return-follows-link)
(defvar org-roam-backlinks-mode)
(defvar org-roam-last-window)
(defvar org-ref-cite-types) ;; in org-ref-core.el
(defvar org-roam-mode)
(defvar org-roam--org-link-bracket-typed-re)

(declare-function org-roam-db--ensure-built   "org-roam-db")
(declare-function org-roam-db--get-title      "org-roam-db")
(declare-function org-roam-db-has-file-p      "org-roam-db")
(declare-function org-roam--extract-refs      "org-roam")
(declare-function org-roam--extract-titles    "org-roam")
(declare-function org-roam--get-backlinks     "org-roam")
(declare-function org-roam-backlinks-mode     "org-roam")
(declare-function org-roam-mode               "org-roam")
(declare-function org-roam--find-file         "org-roam")
(declare-function org-roam-format-link        "org-roam")
(declare-function org-roam-link-get-path      "org-roam-link")

(defcustom org-roam-buffer-position 'right
  "Position of `org-roam' buffer.
Valid values are
 * left,
 * right,
 * top,
 * bottom,
 * a function returning one of the above."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom)
                 function)
  :group 'org-roam)

(defcustom org-roam-buffer-width 0.33
  "Width of `org-roam' buffer.
Has an effect if and only if `org-roam-buffer-position' is `left' or `right'."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer-height 0.27
  "Height of `org-roam' buffer.
Has an effect if and only if `org-roam-buffer-position' is `top' or `bottom'."
  :type 'number
  :group 'org-roam)


(defcustom org-roam-buffer "*org-roam*"
  "Org-roam buffer name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-buffer-prepare-hook '(org-roam-buffer--insert-title
                                          org-roam-buffer--insert-backlinks
                                          org-roam-buffer--insert-ref-links)
  "Hook run in the `org-roam-buffer' before it is displayed."
  :type 'hook
  :group 'org-roam)

(defcustom org-roam-buffer-preview-function #'org-roam-buffer--preview
  "Function to obtain preview contents for a given link.
The function takes in two arguments, the FILE containing the
link, and the POINT of the link."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-buffer-window-parameters nil
  "Additional window parameters for the `org-roam-buffer' side window.
For example: (setq org-roam-buffer-window-parameters '((no-other-window . t)))"
  :type '(alist)
  :group 'org-roam)

(defvar org-roam-buffer--current nil
  "Currently displayed file in `org-roam' buffer.")

(defun org-roam-buffer--find-file (file)
  "Open FILE in the window `org-roam' was called from."
  (setq file (expand-file-name file))
  (let ((last-window org-roam-last-window))
    (if (window-valid-p last-window)
        (progn (with-selected-window last-window
                 (org-roam--find-file file))
               (select-window last-window))
      (org-roam--find-file file))))

(defun org-roam-buffer--insert-title ()
  "Insert the org-roam-buffer title."
  (insert (propertize (org-roam-db--get-title
                       (buffer-file-name org-roam-buffer--current))
                      'font-lock-face
                      'org-document-title)))

(defun org-roam-buffer--preview (file point)
  "Get preview content for FILE at POINT."
  (save-excursion
    (org-roam--with-temp-buffer file
      (goto-char point)
      (let ((elem (org-element-at-point)))
        (or (org-element-property :raw-value elem)
            (when-let ((begin (org-element-property :begin elem))
                       (end (org-element-property :end elem)))
              (string-trim (buffer-substring-no-properties begin end))))))))

(defun org-roam-buffer--pluralize (string number)
  "Conditionally pluralize STRING if NUMBER is above 1."
  (let ((l (pcase number
             ((pred (listp)) (length number))
             ((pred (integerp)) number)
             (wrong-type (signal 'wrong-type-argument
                                 `((listp integerp)
                                   ,wrong-type))))))
    (concat string (when (> l 1) "s"))))

(defun org-roam-buffer-expand-links (content orig-path)
  "Crawl CONTENT for relative links and corrects them to be correctly displayed.
ORIG-PATH is the path where the CONTENT originated."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (link link-type)
      (while (re-search-forward org-roam--org-link-bracket-typed-re (point-max) t)
        (setq link-type (match-string 1)
              link (match-string 2))
        (when (and (string-equal link-type "file")
                   (f-relative-p link))
          (replace-match (org-roam-link-get-path (expand-file-name link (file-name-directory orig-path)))
                         nil t nil 2))))
    (buffer-string)))

(defun org-roam-buffer--insert-ref-links ()
  "Insert ref backlinks for the current buffer."
  (when-let* ((refs (with-temp-buffer
                      (insert-buffer-substring org-roam-buffer--current)
                      (org-roam--extract-refs)))
              (paths (mapcar #'cdr refs)))
    (if-let* ((key-backlinks (mapcan #'org-roam--get-backlinks paths))
              (grouped-backlinks (--group-by (nth 0 it) key-backlinks)))
        (progn
          (insert (let ((l (length key-backlinks)))
                    (format "\n\n* %d %s\n"
                            l (org-roam-buffer--pluralize "Ref Backlink" l))))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** %s\n"
                              (org-roam-format-link file-from
                                                    (org-roam-db--get-title file-from)
                                                    "file")))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (if-let ((content (funcall org-roam-buffer-preview-function file-from (plist-get props :point))))
                              (propertize (org-roam-buffer-expand-links content file-from)
                                          'help-echo "mouse-1: visit backlinked note"
                                          'file-from file-from
                                          'file-from-point (plist-get props :point))
                            "")
                          "\n\n"))))))
      (insert "\n\n* No ref backlinks!"))))

(defun org-roam-buffer--insert-backlinks ()
  "Insert the org-roam-buffer backlinks string for the current buffer."
  (let (props file-from)
    (if-let* ((file-path (buffer-file-name org-roam-buffer--current))
              (titles (with-current-buffer org-roam-buffer--current
                        (org-roam--extract-titles)))
              (backlinks (org-roam--get-backlinks (push file-path titles)))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          (insert (let ((l (length backlinks)))
                    (format "\n\n* %d %s\n"
                            l (org-roam-buffer--pluralize "Backlink" l))))
          (dolist (group grouped-backlinks)
            (setq file-from (car group))
            (setq props (mapcar (lambda (row) (nth 2 row)) (cdr group)))
            (setq props (seq-sort-by (lambda (p) (plist-get p :point)) #'< props))
            (insert (format "** %s\n"
                            (org-roam-format-link file-from
                                                  (org-roam-db--get-title file-from)
                                                  "file")))
            (dolist (prop props)
              (insert "*** "
                      (if-let ((outline (plist-get prop :outline)))
                          (-> outline
                              (string-join " > ")
                              (org-roam-buffer-expand-links file-from))
                        "Top")
                      "\n"
                      (if-let ((content (funcall org-roam-buffer-preview-function file-from (plist-get prop :point))))
                          (propertize
                           (s-trim (s-replace "\n" " " (org-roam-buffer-expand-links content file-from)))
                           'help-echo "mouse-1: visit backlinked note"
                           'file-from file-from
                           'file-from-point (plist-get prop :point))
                        "")
                      "\n\n"))))
      (insert "\n\n* No backlinks!"))))

(defun org-roam-buffer-update ()
  "Update the `org-roam-buffer'."
  (interactive)
  (org-roam-db--ensure-built)
  (let* ((source-org-roam-directory org-roam-directory))
    (with-current-buffer org-roam-buffer
      ;; When dir-locals.el is used to override org-roam-directory,
      ;; org-roam-buffer should have a different local org-roam-directory and
      ;; default-directory, as relative links are relative from the overridden
      ;; org-roam-directory.
      (setq-local org-roam-directory source-org-roam-directory)
      (setq-local default-directory source-org-roam-directory)
      ;; Locally overwrite the file opening function to re-use the
      ;; last window org-roam was called from
      (setq-local org-link-frame-setup
                  (cons '(file . org-roam--find-file) org-link-frame-setup))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (eq major-mode 'org-mode)
          (org-mode))
        (unless org-roam-backlinks-mode
          (org-roam-backlinks-mode))
        (make-local-variable 'org-return-follows-link)
        (setq org-return-follows-link t)
        (run-hooks 'org-roam-buffer-prepare-hook)
        (read-only-mode 1)))))


(cl-defun org-roam-buffer--update-maybe (&key redisplay)
  "Reconstructs `org-roam-buffer'.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
`org-roam-buffer'."
  (let ((buffer (window-buffer)))
    (when (and (or redisplay
                   (not (eq org-roam-buffer--current buffer)))
               (eq 'visible (org-roam-buffer--visibility))
               (buffer-file-name buffer)
               (org-roam-db-has-file-p (buffer-file-name buffer)))
      (setq org-roam-buffer--current buffer)
      (org-roam-buffer-update))))

;;;; Toggling the org-roam buffer
(define-inline org-roam-buffer--visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam-buffer--set-width (width)
  "Set the width of `org-roam-buffer' to `WIDTH'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun org-roam-buffer--set-height (height)
  "Set the height of `org-roam-buffer' to `HEIGHT'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (h (max height window-min-height)))
      (cond
       ((> (window-height) h)
        (shrink-window  (- (window-height) h)))
       ((< (window-height) h)
        (enlarge-window (- h (window-height))))))))

(defun org-roam-buffer--get-create ()
  "Set up the `org-roam' buffer at `org-roam-buffer-position'."
  (let ((position (if (functionp org-roam-buffer-position)
                      (funcall org-roam-buffer-position)
                    org-roam-buffer-position)))
    (save-selected-window
      (-> (get-buffer-create org-roam-buffer)
          (display-buffer-in-side-window
           `((side . ,position)
             (window-parameters . ,org-roam-buffer-window-parameters)))
          (select-window))
      (pcase position
        ((or 'right 'left)
         (org-roam-buffer--set-width
          (round (* (frame-width)  org-roam-buffer-width))))
        ((or 'top  'bottom)
         (org-roam-buffer--set-height
          (round (* (frame-height) org-roam-buffer-height))))))))

(defun org-roam-buffer-activate ()
  "Activate display of the `org-roam-buffer'."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (setq org-roam-last-window (get-buffer-window))
  (org-roam-buffer--get-create))

(defun org-roam-buffer-deactivate ()
  "Deactivate display of the `org-roam-buffer'."
  (interactive)
  (setq org-roam-last-window (get-buffer-window))
  (delete-window (get-buffer-window org-roam-buffer)))

(defun org-roam-buffer-toggle-display ()
  "Toggle display of the `org-roam-buffer'."
  (interactive)
  (pcase (org-roam-buffer--visibility)
    ('visible (org-roam-buffer-deactivate))
    ((or 'exists 'none) (org-roam-buffer-activate))))

(provide 'org-roam-buffer)

;;; org-roam-buffer.el ends here
