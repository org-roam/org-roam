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
(require 'ob-core)  ;for org-babel-parse-header-arguments

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

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like file-name-extension, but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (org-roam--file-name-extension path)))
    (or (string= ext "org")
        (and
         (string= ext "gpg")
         (string= (org-roam--file-name-extension (file-name-sans-extension path)) "org")))))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of org-roam system, return nil otherwise.
If FILE is not specified, use the current-buffer file path."
  (let ((path (or file
                  (buffer-file-name (current-buffer)))))
    (and path
         (org-roam--org-file-p path)
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory)))))

(defun org-roam--aliases-str-to-list (str)
  "Function to transform string STR into list of alias titles.

This snippet is obtained from ox-hugo:
https://github.com/kaushalmodi/ox-hugo/blob/a80b250987bc770600c424a10b3bca6ff7282e3c/ox-hugo.el#L3131"
  (when (stringp str)
    (let* ((str (org-trim str))
           (str-list (split-string str "\n"))
           ret)
      (dolist (str-elem str-list)
        (let* ((format-str ":dummy '(%s)") ;The :dummy key is discarded in the `lst' var below.
               (alist (org-babel-parse-header-arguments (format format-str str-elem)))
               (lst (cdr (car alist)))
               (str-list2 (mapcar (lambda (elem)
                                    (cond
                                     ((symbolp elem)
                                      (symbol-name elem))
                                     (t
                                      elem)))
                                  lst)))
          (setq ret (append ret str-list2))))
      ret)))

;;; -
(provide 'org-roam-utils)
;;; org-roam-utils.el ends here
