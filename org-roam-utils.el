;;; org-roam-utils.el --- Org-roam utility functions  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>

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
;; Provides several utility functions used throughout Org-roam.
;;
;;; Code:

(require 'f)

(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam--touch-file (path)
  "Touches an empty file at PATH."
  (make-directory (file-name-directory path) t)
  (f-touch path))

;;; -
(provide 'org-roam-utils)
;;; org-roam-utils.el ends here
