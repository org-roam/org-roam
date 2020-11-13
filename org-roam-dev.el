;;; org-roam-dev.el --- Org-roam development code -mode -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library provides code for org-roam developers.
;; It is intended to be loaded before editing org-roam source files.
;; It ensures consistent application of various developer settings.
;;
;;; Code:
(require 'emacsql)

;;;###autoload
(define-minor-mode org-roam-dev-mode
  "Minor mode for setting the dev environment of Org-roam."
  :lighter " ORD"
  (when org-roam-dev-mode
    (emacsql-fix-vector-indentation)
    (setq-local sentence-end-double-space nil)))

(provide 'org-roam-dev)
;;; org-roam-dev.el ends here
