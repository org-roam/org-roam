;;; org-roam-dailies.el --- Daily notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.0
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
;; This library provides functionality for creating daily notes. This is a
;; concept borrowed from Roam Research.
;;
;;; Code:
;;; Library Requires
(require 'org-capture)
(require 'org-roam-capture)
(require 'org-roam-macs)

(defvar org-roam-dailies-capture-templates
  '(("d" "daily" plain (function org-roam-capture--get-point)
     ""
     :immediate-finish t
     :file-name "%<%Y-%m-%d>"
     :head "#+title: %<%Y-%m-%d>"))
  "Capture templates for daily notes in Org-roam.")

;; Declarations
(defvar org-roam-mode)
(declare-function org-roam--file-path-from-id "org-roam")
(declare-function org-roam-mode               "org-roam")

(defun org-roam-dailies--file-for-time (time)
  "Create and find file for TIME."
  (let ((org-roam-capture-templates org-roam-dailies-capture-templates)
        (org-roam-capture--info (list (cons 'time time)))
        (org-roam-capture--context 'dailies))
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
    (org-roam--with-template-error 'org-roam-dailies-capture-templates
      (org-roam-capture--capture))))

(defun org-roam-dailies-today ()
  "Create and find the daily note for today."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (org-roam-dailies--file-for-time (current-time)))

(defun org-roam-dailies-tomorrow (n)
  "Create and find the daily note for tomorrow.
With numeric argument N, use N days in the future."
  (interactive "p")
  (unless org-roam-mode (org-roam-mode))
  (org-roam-dailies--file-for-time (time-add (* n 86400) (current-time))))

(defun org-roam-dailies-yesterday (n)
  "Create and find the file for yesterday.
With numeric argument N, use N days in the past."
  (interactive "p")
  (unless org-roam-mode (org-roam-mode))
  (org-roam-dailies-tomorrow (- n)))

(defun org-roam-dailies-date ()
  "Create the file for any date using the calendar interface."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (org-roam-dailies--file-for-time time)))

(provide 'org-roam-dailies)

;;; org-roam-dailies.el ends here
