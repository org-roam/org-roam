;;; org-roam-macs.el --- Macros/utility functions -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library implements macros used throughout org-roam.
;;
;;; Code:
(defmacro org-roam-plist-map! (fn plist)
  "Map FN over PLIST, modifying it in-place."
  (declare (indent 1))
  (let ((plist-var (make-symbol "plist"))
        (k (make-symbol "k"))
        (v (make-symbol "v")))
    `(let ((,plist-var (copy-sequence ,plist)))
       (while ,plist-var
         (setq ,k (pop ,plist-var))
         (setq ,v (pop ,plist-var))
         (setq ,plist (plist-put ,plist ,k (funcall ,fn ,k ,v)))))))

(defmacro org-roam-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (unless (equal major-mode 'org-mode)
         (delay-mode-hooks
           (let ((org-inhibit-startup t)
                 (org-agenda-files nil))
             (org-mode))))
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
       (when (find-buffer-visiting ,file)
         (kill-buffer (find-buffer-visiting ,file))))
     res))

(defmacro org-roam-with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.
Like `with-temp-buffer', but propagates `org-roam-directory'.
If FILE, set `default-directory' to FILE's directory and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory))
           (delay-mode-hooks (org-mode))
           (when ,file
             (insert-file-contents ,file)
             (setq-local default-directory (file-name-directory ,file)))
           ,@body)))))

(provide 'org-roam-macs)

;;; org-roam-macs.el ends here
