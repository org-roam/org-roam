;;; org-roam-refile.el --- Refile Org-roam Notes             -*- lexical-binding: t; -*-

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
;; Org-roam refile allows you to refile notes to your nodes.
;;
;;; Code:
(eval-when-compile
  (require 'org-roam-macs)
  (require 'org-macs))
(require 'org-roam-utils)

(defvar org-auto-align-tags)
(defvar org-loop-over-headlines-in-active-region)

(defun org-roam-demote-entire-buffer ()
  "Convert an org buffer with any top level content to a single node.

All headings are demoted one level.

The #+TITLE: keyword is converted into a level-1 heading and deleted.
Any tags declared on #+FILETAGS: are transferred to tags on the new top heading.

Any top level properties drawers are incorporated into the new heading."
  (interactive)
  (org-with-point-at 1
    (org-map-entries 'org-do-demote)
    (insert "* "
            (org-roam--file-keyword-get "TITLE")
            "\n")
    (org-back-to-heading)
    (org-set-tags (org-roam--file-keyword-get "FILETAGS"))
    (org-roam--file-keyword-kill "TITLE")
    (org-roam--file-keyword-kill "FILETAGS")))

(defun org-roam-refile ()
  "Refile to node."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if regionp
        (progn
          (org-kill-new (buffer-substring region-start region-end))
          (org-save-markers-in-region region-start region-end))
      (progn
        (if (org-before-first-heading-p)
            (org-roam-demote-entire-buffer))
        (org-copy-subtree 1 nil t)))
    (with-current-buffer nbuf
      (org-with-wide-buffer
       (goto-char (org-roam-node-point node))
       (setq level (org-get-valid-level (funcall outline-level) 1)
             reversed (org-notes-order-reversed-p))
       (goto-char
        (if reversed
            (or (outline-next-heading) (point-max))
          (or (save-excursion (org-get-next-sibling))
              (org-end-of-subtree t t)
              (point-max))))
       (unless (bolp) (newline))
       (org-paste-subtree level nil nil t)
       (and org-auto-align-tags
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
       (when (fboundp 'deactivate-mark) (deactivate-mark))))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
    (org-roam--kill-empty-buffer)
    ))

(provide 'org-roam-refile)
;;; org-roam-refile.el ends here
