;;; org-roam-link.el --- Custom links for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;;                  Alan Carroll

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
;; This adds the custom `roam:' link to Org-roam. `roam:' links allow linking to
;; Org-roam files via their titles and headlines.
;;
;;; Code:
;;;; Dependencies

(require 'ol)
(require 'org-roam-compat)
(require 'org-roam-macs)
(require 'org-roam-db)

(require 'org-element)

(defvar org-roam-completion-ignore-case)
(defvar org-roam-directory)
(declare-function  org-roam--find-file                  "org-roam")
(declare-function  org-roam-find-file                   "org-roam")
(declare-function org-roam-format-link                  "org-roam")

(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file or id links whenever possible."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-link-file-path-type 'relative
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory."
  :group 'org-roam
  :type '(choice
          (const relative)
          (const absolute)
          (const noabbrev))
  :safe #'symbolp)

;;; the roam: link
(org-link-set-parameters "roam"
                         :follow #'org-roam-link-follow-link)

(defun org-roam-link-follow-link (_path)
  "Navigates to location in Org-roam link.
This function is called by Org when following links of the type
`roam'. While the path is passed, assume that the cursor is on
the link."
  (pcase-let ((`(,link-type ,loc ,desc ,mkr) (org-roam-link--get-location)))
    (when (and org-roam-link-auto-replace loc desc)
      (org-roam-link--replace-link link-type loc desc))
    (pcase link-type
      ("file"
       (if loc
           (org-roam--find-file loc)
         (org-roam-find-file desc nil nil t)))
      ("id"
       (org-goto-marker-or-bmk mkr)))))

;;; Retrieval Functions
(defun org-roam-link--get-titles ()
  "Return all titles within Org-roam."
  (mapcar #'car (org-roam-db-query [:select [titles:title] :from titles])))

(defun org-roam-link--get-headlines (&optional file with-marker use-stack)
  "Return all outline headings for the current buffer.
If FILE, return outline headings for passed FILE instead.
If WITH-MARKER, return a cons cell of (headline . marker).
If USE-STACK, include the parent paths as well."
  (org-roam-with-file file (when with-marker 'keep)
    (let* ((outline-level-fn outline-level)
           (path-separator "/")
           (stack-level 0)
           stack cands name level marker)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-complex-heading-regexp nil t)
          (save-excursion
            (setq name (substring-no-properties (or (match-string 4) "")))
            (setq marker (point-marker))
            (when use-stack
              (goto-char (match-beginning 0))
              (setq level (funcall outline-level-fn))
              ;; Update stack.  The empty entry guards against incorrect
              ;; headline hierarchies, e.g. a level 3 headline
              ;; immediately following a level 1 entry.
              (while (<= level stack-level)
                (pop stack)
                (cl-decf stack-level))
              (while (> level stack-level)
                (push name stack)
                (cl-incf stack-level))
              (setq name (mapconcat #'identity
                                    (reverse stack)
                                    path-separator)))
            (push (if with-marker
                      (cons name marker)
                    name) cands))))
      (nreverse cands))))


(defun org-roam-link--get-file-from-title (title &optional no-interactive)
  "Return the file path corresponding to TITLE.
When NO-INTERACTIVE, return nil if there are multiple options."
  (let ((files (mapcar #'car (org-roam-db-query [:select [titles:file] :from titles
                                                 :where (= titles:title $v1)]
                                                (vector title)))))
    (pcase files
      ('nil nil)
      (`(,file) file)
      (_
       (unless no-interactive
         (completing-read "Select file: " files))))))

(defun org-roam-link--get-id-from-headline (headline &optional file)
  "Return (marker . id) correspondng to HEADLINE in FILE.
If FILE is nil, get ID from current buffer.
If there is no corresponding headline, return nil."
  (save-excursion
    (org-roam-with-file file 'keep
      (let ((headlines (org-roam-link--get-headlines file 'with-markers)))
        (when-let ((marker (cdr (assoc-string headline headlines))))
          (goto-char marker)
          (cons marker
                (when org-roam-link-auto-replace
                  (org-id-get-create))))))))

;;; Path-related functions
(defun org-roam-link-get-path (path &optional type)
  "Return the PATH of the link to use.
If TYPE is non-nil, create a link of TYPE. Otherwise, respect
`org-link-file-path-type'."
  (pcase (or type org-roam-link-file-path-type)
      ('absolute
       (abbreviate-file-name (expand-file-name path)))
      ('noabbrev
       (expand-file-name path))
      ('relative
       (file-relative-name path))))

(defun org-roam-link--split-path (path)
  "Splits PATH into title and headline.
Return a list of the form (type title has-headline-p headline star-idx).
type is one of `title', `headline', `title+headline'.
title is the title component of the path.
headline is the headline component of the path.
star-idx is the index of the asterisk, if any."
  (save-match-data
    (let* ((star-index (string-match-p "\\*" path))
           (title (substring-no-properties path 0 star-index))
           (headline (if star-index
                         (substring-no-properties path (+ 1 star-index))
                       ""))
           (type (cond ((not star-index)
                        'title)
                       ((= 0 star-index)
                        'headline)
                       (t 'title+headline))))
      (list type title headline star-index))))

(defun org-roam-link--get-location ()
  "Return the location of the Org-roam fuzzy link at point.
The location is returned as a list containing (link-type loc desc marker).
nil is returned if there is no matching location.

link-type is either \"file\" or \"id\".
loc is the target location: e.g. a file path, or an id.
marker is a marker to the headline, if applicable.

desc is either the the description of the link under point, or
the target of LINK (title or heading content)."
  (let ((context (org-element-context))
        mkr link-type desc loc)
    (pcase (org-element-lineage context '(link) t)
      (`nil (error "Not at an Org link"))
      (link
       (if (not (string-equal "roam" (org-element-property :type link)))
           (error "Not at Org-roam link")
         (setq desc (and (org-element-property :contents-begin link)
                         (org-element-property :contents-end link)
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin link)
                          (org-element-property :contents-end link))))
         (pcase-let ((`(,type ,title ,headline _) (org-roam-link--split-path
                                                   (org-element-property :path link))))
           (pcase type
             ('title+headline
              (let ((file (org-roam-link--get-file-from-title title)))
                (if (not file)
                    (org-roam-message "Cannot find matching file")
                  (setq mkr (org-roam-link--get-id-from-headline headline file))
                  (pcase mkr
                    (`(,marker . ,target-id)
                     (progn
                       (setq mkr marker
                             loc target-id
                             desc (or desc headline)
                             link-type "id")))
                    (_ (org-roam-message "Cannot find matching id"))))))
             ('title
              (setq loc (org-roam-link--get-file-from-title title)
                    link-type "file"
                    desc (or desc title)))
             ('headline
              (setq mkr (org-roam-link--get-id-from-headline headline))
              (pcase mkr
                (`(,marker . ,target-id)
                 (setq mkr marker
                       loc target-id
                       link-type "id"
                       desc (or desc headline)))
                (_ (org-roam-message "Cannot find matching headline")))))))))
    (list link-type loc desc mkr)))

;;; Conversion Functions
(defun org-roam-link--replace-link (link-type loc &optional desc)
  "Replace link at point with a vanilla Org link.
LINK-TYPE is the Org link type, typically \"file\" or \"id\".
LOC is path for the Org link.
DESC is the link description."
  (save-excursion
    (save-match-data
      (unless (org-in-regexp org-link-bracket-re 1)
        (user-error "No link at point"))
      (replace-match "")
      (insert (org-roam-format-link loc desc link-type)))))

(defun org-roam-link-replace-all ()
  "Replace all roam links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (condition-case nil
          (pcase-let ((`(,link-type ,loc ,desc _) (org-roam-link--get-location)))
            (when (and link-type loc)
              (org-roam-link--replace-link link-type loc desc)))
        (error nil)))))

(defun org-roam-link--replace-link-on-save ()
  "Hook to replace all roam links on save."
  (when org-roam-link-auto-replace
    (org-roam-link-replace-all)))

;;; Completion
(defun org-roam-link-complete-at-point ()
  "Do appropriate completion for the link at point."
  (let ((end (point))
        (start (point))
        collection link-type headline-only-p)
    (when (org-in-regexp org-link-bracket-re 1)
      (setq start (match-beginning 1)
            end (match-end 1))
      (let ((context (org-element-context)))
        (pcase (org-element-lineage context '(link) t)
          (`nil nil)
          (link
           (setq link-type (org-element-property :type link))
           (when (member link-type '("roam" "fuzzy"))
             (when (string= link-type "roam") (setq start (+ start (length "roam:"))))
             (pcase-let ((`(,type ,title _ ,star-idx)
                          (org-roam-link--split-path (org-element-property :path link))))
               (pcase type
                 ('title+headline
                  (when-let ((file (org-roam-link--get-file-from-title title t)))
                    (setq collection (apply-partially #'org-roam-link--get-headlines file))
                    (setq start (+ start star-idx 1))))
                 ('title
                  (setq collection #'org-roam-link--get-titles))
                 ('headline
                  (setq collection #'org-roam-link--get-headlines)
                  (setq start (+ start star-idx 1))
                  (setq headline-only-p t)))))))))
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
                (delete-char (- 0 (length str)
                                (if headline-only-p 1 0)))
                (insert (concat (unless (string= link-type "roam") "roam:")
                                (when headline-only-p "*")
                                (org-link-escape str)))))))))

(provide 'org-roam-link)
;;; org-roam-link.el ends here
