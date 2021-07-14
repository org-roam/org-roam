;;; org-roam-completion.el --- Completion features -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library provides completion-at-point functions for Org-roam.
;;
;; The two main functions provided to capf are:
;;
;;    `org-roam-complete-link-at-point' provides completions to nodes
;;    within link brackets
;;
;;    `org-roam-complete-everywhere' provides completions for nodes everywhere,
;;    matching the symbol at point
;;
;;; Code:
(require 'cl-lib)
(require 'org-element)

(declare-function org-roam--get-titles "org-roam")

(defcustom org-roam-completion-everywhere nil
  "When non-nil, provide link completion matching outside of Org links."
  :group 'org-roam
  :type 'boolean)

(defvar org-roam-completion-functions (list #'org-roam-complete-link-at-point
                                            #'org-roam-complete-everywhere)
  "List of functions to be used with `completion-at-point' for Org-roam.")

(defconst org-roam-bracket-completion-re
  "\\[\\[\\(\\(?:roam:\\)?\\)\\([^z-a]*\\)]]"
  "Regex for completion within link brackets.
We use this as a substitute for `org-link-bracket-re', because
`org-link-bracket-re' requires content within the brackets for a match.")

(defun org-roam-complete-everywhere ()
  "Provides completions for links for any word at point.
This is a `completion-at-point' function, and is active when
`org-roam-completion-everywhere' is non-nil."
  (when (and org-roam-completion-everywhere
             (thing-at-point 'word)
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (lambda (_)
               (funcall #'org-roam--get-titles)))
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert "[[roam:" str "]]"))))))

(defun org-roam-complete-link-at-point ()
  "Do appropriate completion for the link at point."
  (let (roam-p start end)
    (when (org-in-regexp org-roam-bracket-completion-re 1)
      (setq roam-p (not (string-blank-p (match-string 1)))
            start (match-beginning 2)
            end (match-end 2))
      (list start end
            (completion-table-dynamic
             (lambda (_)
               (funcall #'org-roam--get-titles)))
            :exit-function
            (lambda (str &rest _)
              (delete-char (- 0 (length str)))
              (insert (concat (unless roam-p "roam:")
                              str))
              (forward-char 2))))))

(defun org-roam-complete-at-point ()
  "."
  (run-hook-with-args-until-success 'org-roam-completion-functions))

(defun org-roam--register-completion-functions ()
  "."
  (add-hook 'completion-at-point-functions #'org-roam-complete-at-point nil t))

(add-hook 'org-roam-find-file-hook #'org-roam--register-completion-functions)

(provide 'org-roam-completion)

;;; org-roam-completion.el ends here
