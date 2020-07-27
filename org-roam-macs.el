;;; org-roam-macs.el --- Macros/utility functions -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.0"))

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

(defvar org-roam-verbose)

(defmacro org-roam--with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.
Like `with-temp-buffer', but propagates `org-roam-directory'.
If FILE, set `org-roam-temp-file-name' to file and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory)
               (org-mode-hook nil))
           (org-mode)
           (when ,file
             (insert-file-contents ,file)
             (setq-local org-roam-file-name ,file))
           ,@body)))))

(defmacro org-roam--with-template-error (templates &rest body)
  "Eval BODY, and point to TEMPLATES on error.
Provides more informative error messages so that users know where
to look.

\(fn TEMPLATES BODY...)"
  (declare (debug (form body)) (indent 1))
  `(condition-case err
       ,@body
     (error (user-error "%s.  Please adjust `%s'"
                        (error-message-string err)
                        ,templates))))

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
(defcustom org-roam-shield-strings '("[[" . "]]")
  "Strings used to shield the region text."
  :group 'org-roam
  :type '(cons (string :tag "Opening characters")
               (string :tag "Closing characters")))

(defun org-roam-shield-region (region)
  "Shield REGION against modifications.
REGION must be a cons-cell containing the marker to the region
beginning and maximum values.

REGION is padded with strings given by `org-roam-shield-strings',
and the new region is returned."
  (when region
    (pcase-let ((`(,min . ,max) region)
                (`(,s-start . ,s-end) org-roam-shield-strings))
      (with-current-buffer (marker-buffer min)
        (save-excursion
          (goto-char min)
          (insert s-start)
          (goto-char max)
          (insert s-end))
        (set-marker max (+ max (length s-end)))
        (add-text-properties min max '(read-only t))
        (cons min max)))))

(defun org-roam-unshield-region (region)
  "Unshield the shielded REGION and returns the unshielded region.
This function assumes that REGION was shielded by `org-roam-shield-region'."
  (when region
    (pcase-let ((`(,min . ,max) region)
                (`(,s-start . ,s-end) org-roam-shield-strings))
      (with-current-buffer (marker-buffer min)
        (let ((shielded-text (buffer-substring min max))
              (inhibit-read-only t))
          (unless (and (string-prefix-p s-start shielded-text)
                       (string-suffix-p s-end shielded-text))
            (error "Region was not shielded by `org-roam-shield-region'"))
          (remove-text-properties min max '(read-only t))
          (save-excursion
            (goto-char min)
            (delete-char (length s-start))
            (goto-char max)
            (delete-char (- (length s-end))))
          (cons min max))))))

(provide 'org-roam-macs)

;;; org-roam-macs.el ends here
