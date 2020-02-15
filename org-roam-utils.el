;;; org-roam-utils.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'cl-lib)

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (file-name-extension path)))
    (or (string= ext "org")
        (and
         (string= ext "gpg")
         (string= (file-name-extension (file-name-sans-extension path)) "org")))))

(defun org-roam--find-files (dir)
  "Return all `org-roam' files in `DIR'."
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            (dir-ignore-regexp (concat "\\(?:"
                                       "\\."
                                       "\\|\\.\\."
                                       "\\)$"))
            result)
        (dolist (file files)
          (cond
           ((file-directory-p file)
            (when (not (string-match dir-ignore-regexp file))
              (setq result (append (org-roam--find-files file) result))))
           ((and (file-readable-p file)
                 (org-roam--org-file-p file))
            (setq result (cons (file-truename file) result)))))
        result)))

(defun org-roam--parse-content (&optional file-path)
  "Parse the current buffer, and return a list of items for processing."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link))
            (start (org-element-property :begin link)))
        (when (and (string= type "file")
                   (org-roam--org-file-p path))
          (goto-char start)
          (let* ((element (org-element-at-point))
                 (content (or (org-element-property :raw-value element)
                              (buffer-substring
                               (or (org-element-property :content-begin element)
                                   (org-element-property :begin element))
                               (or (org-element-property :content-end element)
                                   (org-element-property :end element)))))
                 (content (string-trim content))
                 (file-path (or file-path
                                (file-truename (buffer-file-name (current-buffer))))))
            (list :from file-path
                  :to (file-truename (expand-file-name path (file-name-directory file-path)))
                  :content content)))))))

(cl-defun org-roam--insert-item (item &key forward backward)
  "Insert ITEM into FORWARD and BACKWARD cache.

ITEM is of the form: (:from from-path :to to-path :content preview-content)."
  (pcase-let ((`(:from ,p-from :to ,p-to :content ,content) item))
    ;; Build forward-links
    (let ((links (gethash p-from forward)))
      (if links
          (puthash p-from
                   (if (member p-to links)
                       links
                     (cons p-to links)) forward)
        (puthash p-from (list p-to) forward)))
    ;; Build backward-links
    (let ((contents-hash (gethash p-to backward)))
      (if contents-hash
          (if-let ((contents-list (gethash p-from contents-hash)))
              (let ((updated (cons content contents-list)))
                (puthash p-from updated contents-hash)
                (puthash p-to contents-hash backward))
            (progn
              (puthash p-from (list content) contents-hash)
              (puthash p-to contents-hash backward)))
        (let ((contents-hash (make-hash-table :test #'equal)))
          (puthash p-from (list content) contents-hash)
          (puthash p-to contents-hash backward))))))

(defun org-roam--extract-title ()
  "Extract the title from `BUFFER'."
  (org-element-map
      (org-element-parse-buffer)
      'keyword
    (lambda (kw)
      (when (string= (org-element-property :key kw) "TITLE")
        (org-element-property :value kw)))
    :first-match t))

(provide 'org-roam-utils)

;;; org-roam-utils.el ends here
