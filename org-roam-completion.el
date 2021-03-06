;;; org-roam-completion.el --- Completion features -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library provides completion-at-point for org-roam.
;;; Code:
(require 'cl-lib)

(defcustom org-roam-completion-ignore-case t
  "Whether to ignore case in Org-roam `completion-at-point' completions."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-completion-everywhere nil
  "When non-nil, provide link completion matching outside of Org links.")

(defvar org-roam-completion-functions (list #'org-roam-complete-link-at-point
                                            #'org-roam-complete-everywhere)
  "List of functions to be used with `completion-at-point' for Org-roam.")

(defun org-roam-complete-everywhere ()
  "Provides completions for links for any word at point.
This is a `completion-at-point' function, and is active when
`org-roam-completion-everywhere' is non-nil."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (and org-roam-completion-everywhere
               (thing-at-point 'word)
               (not (save-match-data (org-in-regexp org-link-any-re))))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (setq start (car bounds)
              end (cdr bounds)
              collection #'org-roam--get-titles
              exit-fn (lambda (str _status)
                        (delete-char (- (length str)))
                        (insert "[[roam:" str "]]")))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function exit-fn)))))

(defun org-roam-complete-link-at-point ()
  "Do appropriate completion for the link at point."
  (let ((end (point))
        (start (point))
        collection path)
    (when (org-in-regexp org-link-bracket-re 1)
      (setq start (match-beginning 1)
            end (match-end 1))
      (let ((context (org-element-context)))
        (pcase (org-element-lineage context '(link) t)
          (`nil nil)
          (link
           (setq link-type (org-element-property :type link)
                 path (org-element-property :path link))
           (when (member link-type '("roam" "fuzzy"))
             (when (string= link-type "roam") (setq start (+ start (length "roam:"))))
             (setq collection #'org-roam-link--get-nodes))))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function
              (lambda (str &rest _)
                (delete-char (- 0 (length str)))
                (insert (concat (unless (string= link-type "roam") "roam:")
                                str))
                (forward-char 2)))))))

(provide 'org-roam-completion)

;;; org-roam-completion.el ends here
