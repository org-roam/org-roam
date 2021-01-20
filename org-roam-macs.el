;;; org-roam-macs.el --- Macros/utility functions -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library implements macros and utility functions used throughout
;; org-roam.
;;
;;
;;; Code:
;;;; Library Requires
(require 'dash)
(require 's)

(defvar org-roam-verbose)

;; This is necessary to ensure all dependents on this module see
;; `org-mode-hook' and `org-inhibit-startup' as dynamic variables,
;; regardless of whether Org is loaded before their compilation.
(require 'org)

;;;; Utility Functions
(defun org-roam--list-interleave (lst separator)
  "Interleaves elements in LST with SEPARATOR."
  (when lst
    (let ((new-lst (list (pop lst))))
      (dolist (it lst)
        (nconc new-lst (list separator it)))
      new-lst)))

(defmacro org-roam-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
       (when (find-buffer-visiting ,file)
         (kill-buffer (find-buffer-visiting ,file))))
     res))

(defmacro org-roam--with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.
Like `with-temp-buffer', but propagates `org-roam-directory'.
If FILE, set `org-roam-temp-file-name' to file and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory)
               (org-mode-hook nil)
               (org-inhibit-startup t))
           (org-mode)
           (when ,file
             (insert-file-contents ,file)
             (setq-local org-roam-file-name ,file)
             (setq-local default-directory (file-name-directory ,file)))
           ,@body)))))

(defun org-roam-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `org-roam-verbose' is t."
  (when org-roam-verbose
    (apply #'message `(,(concat "(org-roam) " format-string) ,@args))))

(defun org-roam-string-quote (str)
  "Quote STR."
  (->> str
       (s-replace "\\" "\\\\")
       (s-replace "\"" "\\\"")))

;;; Shielding regions
(defun org-roam-shield-region (beg end)
  "Shield REGION against modifications.
REGION must be a cons-cell containing the marker to the region
beginning and maximum values."
  (when (and beg end)
    (add-text-properties beg end
                           '(font-lock-face org-roam-link-shielded
                                            read-only t)
                           (marker-buffer beg))
    (cons beg end)))

(defun org-roam-unshield-region (beg end)
  "Unshield the shielded REGION."
  (when (and beg end)
    (let ((inhibit-read-only t))
      (remove-text-properties beg end
                              '(font-lock-face org-roam-link-shielded
                                               read-only t)
                              (marker-buffer beg)))
    (cons beg end)))

(provide 'org-roam-macs)

;;; org-roam-macs.el ends here
