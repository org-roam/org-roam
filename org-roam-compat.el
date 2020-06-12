;;; org-roam-compat.el --- Compatibility Code -*- coding: utf-8; lexical-binding: t; -*-

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
;; This file contains code needed for backward compatibility with older Emacsen
;; and previous versions of org-roam.
;;
;;; Code:
;;;; Library Requires

;;; Obsolete aliases (remove after next major release)
;;;; Functions
(define-obsolete-function-alias 'org-roam--capture-get-point 'org-roam-capture--get-point
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-build-cache 'org-roam-db-build-cache
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-sql         'org-roam-db-query
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--get-db     'org-roam-db--get
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--db-clear   'org-roam-db--clear
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-show-graph  'org-roam-graph-show
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--maybe-update-buffer
  'org-roam-buffer--update-maybe "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--current-visibility
  'org-roam-buffer--visibility "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-update         'org-roam-buffer-update
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--set-width     'org-roam-buffer--set-width
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--set-height    'org-roam-buffer--set-height
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam--set-up-buffer 'org-roam-buffer--get-create
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-today 'org-roam-dailies-today
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-tomorrow 'org-roam-dailies-tomorrow
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-yesterday 'org-roam-dailies-yesterday
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-date 'org-roam-dailies-date
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-graph-show  'org-roam-graph
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-graph-build 'org-roam-graph
  "org-roam 1.0.0")
(define-obsolete-function-alias 'org-roam-find-index 'org-roam-jump-to-index
  "org-roam 1.1.0")
(define-obsolete-function-alias 'org-roam--pluralize 'org-roam-buffer--pluralize
  "org-roam 1.1.0")
(define-obsolete-function-alias 'org-roam--capture 'org-roam-capture--capture
  "org-roam 1.1.0")
(define-obsolete-function-alias 'org-roam-db--maybe-update 'org-roam-db--update-maybe
  "org-roam 1.1.0")

(when (version< (org-version) "9.3")
  (defalias 'org-link-make-string 'org-make-link-string))

;;;; Variables
(define-obsolete-variable-alias 'org-roam-graphviz-extra-options
  'org-roam-graph-extra-config "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam-grapher-extra-options
  'org-roam-graph-extra-config "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam-graph-node-shape
  'org-roam-graph-node-extra-config "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam--db-connection
  'org-roam-db--connection "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam--current-buffer
  'org-roam-buffer--current "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam-date-title-format
  'org-roam-dailies-capture-templates "org-roam 1.0.0")
(define-obsolete-variable-alias 'org-roam-date-filename-format
  'org-roam-dailies-capture-templates "org-roam 1.0.0")
(make-obsolete-variable 'org-roam-buffer-no-delete-other-windows
  'org-roam-buffer-window-parameters "org-roam 1.1.1")

(provide 'org-roam-compat)

;;; org-roam-compat.el ends here
