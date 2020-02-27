;;; org-roam-utils.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (org "9.0"))

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
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'subr-x)
(require 'cl-lib)
(require 'org-roam-db)

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

(defun org-roam--get-links (&optional file-path)
  "Get the links in the buffer.
If FILE-PATH is passed, use that as the source file."
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name (current-buffer))))))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link))
              (start (org-element-property :begin link)))
          (when (and (string= type "file")
                     (org-roam--org-file-p path))
            (goto-char start)
            (let* ((element (org-element-at-point))
                   (begin (or (org-element-property :content-begin element)
                              (org-element-property :begin element)))
                   (content (or (org-element-property :raw-value element)
                                (buffer-substring
                                 begin
                                 (or (org-element-property :content-end element)
                                     (org-element-property :end element)))))
                   (content (string-trim content)))
              (vector file-path
                      (file-truename (expand-file-name path (file-name-directory file-path)))
                      (list :content content :point begin)))))))))

(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current buffer."
  (let ((buf (org-element-parse-buffer))
        (res '()))
    (dolist (prop props)
      (let ((p (org-element-map
                   buf
                   'keyword
                 (lambda (kw)
                   (when (string= (org-element-property :key kw) prop)
                     (org-element-property :value kw)))
                 :first-match t)))
        (setq res (cons (cons prop p) res))))
    res))

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

(defun org-roam--extract-titles ()
  "Extract the titles from current buffer.
Titles are obtained via the #+TITLE property, or aliases
specified via the #+ROAM_ALIAS property."
  (let* ((props (org-roam--extract-global-props '("TITLE" "ROAM_ALIAS")))
         (aliases (cdr (assoc "ROAM_ALIAS" props)))
         (title (cdr (assoc "TITLE" props)))
         (alias-list (org-roam--aliases-str-to-list aliases)))
    (if title
        (cons title alias-list)
      alias-list)))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer."
  (cdr (assoc "ROAM_KEY" (org-roam--extract-global-props '("ROAM_KEY")))))

(defun org-roam--insert-links (links)
  "Insert LINK into the org-roam cache."
  (org-roam-sql
   [:insert :into file-links
    :values $v1]
   links))

(defun org-roam--insert-titles (file titles)
  "Insert TITLES into the org-roam-cache."
  (org-roam-sql
   [:insert :into titles
    :values $v1]
   (list (vector file titles))))

(defun org-roam--insert-ref (file ref)
  "Insert REF into the Org-roam cache."
  (org-roam-sql
   [:insert :into refs
    :values $v1]
   (list (vector ref file))))

(defun org-roam--clear-cache ()
  "Clears all entries in the caches."
  (interactive)
  (when (file-exists-p (org-roam--get-db))
    (org-roam-sql [:delete :from files])
    (org-roam-sql [:delete :from titles])
    (org-roam-sql [:delete :from file-links])
    (org-roam-sql [:delete :from files])
    (org-roam-sql [:delete :from refs])))

(defun org-roam--clear-file-from-cache (&optional filepath)
  "Remove any related links to the file at FILEPATH.
This is equivalent to removing the node from the graph."
  (let* ((path (or filepath
                   (buffer-file-name (current-buffer))))
         (file (file-truename path)))
    (org-roam-sql [:delete :from files
                   :where (= file $s1)]
                  file)
    (org-roam-sql [:delete :from file-links
                   :where (= file-from $s1)]
                  file)
    (org-roam-sql [:delete :from titles
                   :where (= file $s1)]
                  file)
    (org-roam-sql [:delete :from refs
                   :where (= file $s1)]
                  file)))

(defun org-roam--get-current-files ()
  "Return a hash of file to buffer string hash."
  (let* ((current-files (org-roam-sql [:select * :from files]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam--build-cache ()
  "Build the org-roam for `org-roam-directory'."
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((org-roam-files (org-roam--find-files org-roam-directory))
         (current-files (org-roam--get-current-files))
         (time (current-time))
         all-files all-links all-titles all-refs)
    (dolist (file org-roam-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((contents-hash (secure-hash 'sha1 (current-buffer))))
          (unless (string= (gethash file current-files)
                           contents-hash)
            (org-roam--clear-file-from-cache file)
            (setq all-files
                  (cons (vector file contents-hash time) all-files))
            (when-let (links (org-roam--get-links file))
              (setq all-links (append links all-links)))
            (let ((titles (org-roam--extract-titles)))
              (setq all-titles (cons (vector file titles) all-titles)))
            (when-let ((ref (org-roam--extract-ref)))
              (setq all-refs (cons (vector ref file) all-refs))))
          (remhash file current-files))))
    (dolist (file (hash-table-keys current-files))
      ;; These files are no longer around, remove from cache...
      (org-roam--clear-file-from-cache file))
    (when all-files
      (org-roam-sql
       [:insert :into files
        :values $v1]
       all-files))
    (when all-links
      (org-roam-sql
       [:insert :into file-links
        :values $v1]
       all-links))
    (when all-titles
      (org-roam-sql
       [:insert :into titles
        :values $v1]
       all-titles))
    (when all-refs
      (org-roam-sql
       [:insert :into refs
        :values $v1]
       all-refs))
    (list :files (length all-files)
          :links (length all-links)
          :titles (length all-titles)
          :refs (length all-refs)
          :deleted (length (hash-table-keys current-files)))))

(provide 'org-roam-utils)

;;; org-roam-utils.el ends here
