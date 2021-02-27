;;; org-roam-link.el --- Custom links for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;;                  Alan Carroll

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (magit-section "2.90.1"))

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
;; This adds the custom `roam:' link to Org-roam. `roam:' links allow linking to
;; Org-roam files via the node titles.
;;
;;; Code:
;;;; Dependencies

(require 'ol)
(require 'org-roam-compat)
(eval-when-compile
  (require 'org-roam-macs))
(require 'org-roam-db)

(require 'org-element)

(defvar org-roam-completion-ignore-case)
(defvar org-roam-directory)

(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file or id links whenever possible."
  :group 'org-roam
  :type 'boolean)

;;; the roam: link
(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link)

(defun org-roam-link-follow-link (path)
  "Navigates to roam: link with description PATH.
This function is called by Org when following links of the type
`roam'. While the path is passed, assume that the cursor is on
the link."
  (pcase-let ((`(,id ,file ,pos) (org-roam-link-locate)))
    (when org-roam-link-auto-replace
      (org-roam-link--replace-link id path))
    (org-id-goto id)))

(defun org-roam-link--replace-link (id &optional desc)
  "Replace link at point with a vanilla Org link.
LINK-TYPE is the Org link type, typically \"file\" or \"id\".
ID is id for the Org-roam node.
DESC is the link description."
  (save-excursion
    (save-match-data
      (unless (org-in-regexp org-link-bracket-re 1)
        (user-error "No link at point"))
      (replace-match "")
      (insert (org-link-make-string
               (concat "id:" id)
               desc)))))

(defun org-roam-link-locate ()
  "Return the location of the roam link at point.
This is a list of three items: the node id, the file, and point
in the file."
  (let ((context (org-element-context))
        path matches)
    (pcase (org-element-lineage context '(link) t)
      ('nil (error "Not at Org link"))
      (link
       (if (not (string-equal "roam" (org-element-property :type link)))
           (error "Not at an Org-roam link")
         (setq path (org-element-property :path link))
         (setq matches (seq-uniq
                        (append
                         (org-roam-db-query [:select [id file pos] :from nodes
                                             :where (= title $s1)]
                                            path)
                         (org-roam-db-query [:select [node-id aliases:file nodes:pos] :from aliases
                                             :left :join nodes :on (= nodes:id aliases:node-id)
                                             :where (= aliases:node-id $s1)]
                                            path))))
         (cond
          ((seq-empty-p matches)
           ;; TODO: prompt to capture new note.
           (message "No matches."))
          ((= 1 (length matches))
           (car matches))
          (_
           ;; TODO: need to fix UX somehow
           (let ((choice (completing-read "Choose node:" matches nil t)))
             (cdr (assoc choice matches #'string-equal))))))))))

;;; Retrieval Functions
(defun org-roam-link--get-nodes ()
  "Return all node title and aliases."
  (append
   (mapcar #'car (org-roam-db-query [:select [title] :from nodes]))
   (mapcar #'car (org-roam-db-query [:select [alias] :from aliases]))))

(defun org-roam-link--get-node-from-title (title)
  "Return the node id for a given TITLE."
  (let ((nodes (seq-uniq
                (append
                 (mapcar #'car (org-roam-db-query [:select [id] :from nodes
                                                   :where (= title $s1)]
                                                  title))
                 (mapcar #'car (org-roam-db-query [:select [node-id] :from aliases
                                                   :where (= node-id $s1)]
                                                  title))))))
    (pcase nodes
      ('nil nil)
      (`(,node) node)
      (_
       (completing-read "Select node: " nodes)))))

;;; Completion
(defun org-roam-link-complete-at-point ()
  "Do appropriate completion for the link at point."
  (let ((end (point))
        (start (point))
        collection path)
    (when (org-in-regexp org-link-bracket-re 1)
      (setq start (match-beginning 1)
            end (match-end 1))
      (let ((context (org-element-context)))
        (pcase (org-element-lineage context '(link) t)
          (`nil nil)
          (link
           (setq link-type (org-element-property :type link)
                 path (org-element-property :path link))
           (when (member link-type '("roam" "fuzzy"))
             (when (string= link-type "roam") (setq start (+ start (length "roam:"))))
             (setq collection #'org-roam-link--get-nodes))))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function
              (lambda (str &rest _)
                (delete-char (- 0 (length str)))
                (insert (concat (unless (string= link-type "roam") "roam:")
                                str))
                (forward-char 2)))))))

(provide 'org-roam-link)
;;; org-roam-link.el ends here
