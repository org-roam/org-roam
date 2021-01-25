;;; org-roam-doctor.el --- Linter for Org-roam files -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
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
;; This library provides `org-roam-doctor', a utility for diagnosing and fixing
;; Org-roam files. Running `org-roam-doctor' launches a list of checks defined
;; by `org-roam-doctor--checkers'. Every checker is an instance of
;; `org-roam-doctor-checker'.
;;
;; Each checker is given the Org parse tree (AST), and is expected to return a
;; list of errors. The checker can also provide "actions" for auto-fixing errors
;; (see `org-roam-doctor--remove-link' for an example).
;;
;; The UX experience is inspired by both org-lint and checkdoc, and their code
;; is heavily referenced.
;;
;;; Code:
;; Library Requires
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 's)
(require 'dash)
(require 'org-roam-macs)

(declare-function org-roam-insert "org-roam")
(declare-function org-roam--get-roam-buffers "org-roam")
(declare-function org-roam--list-all-files "org-roam")
(declare-function org-roam--org-roam-file-p "org-roam")
(declare-function org-roam-mode "org-roam")

(defvar org-roam-verbose)
(defvar org-roam-mode)

(defcustom org-roam-doctor-inhibit-startup t
  "Inhibit `org-mode' startup when processing files with `org-doctor'.
When non-nil, images and LaTeX preview will not be generated,
tables will not be aligned, and headlines will not respect
startup visability. This significantly improves performance when
processing multiple files"
  :type 'boolean
  :group 'org-roam)

(cl-defstruct (org-roam-doctor-checker (:copier nil))
  (name 'missing-checker-name)
  (description "")
  (actions nil))

(defconst org-roam-doctor--checkers
  (list
   (make-org-roam-doctor-checker
    :name 'org-roam-doctor-broken-links
    :description "Fix broken links."
    :actions '(("d" . ("Unlink" . org-roam-doctor--remove-link))
               ("r" . ("Replace link" . org-roam-doctor--replace-link))
               ("R" . ("Replace link (keep label)" . org-roam-doctor--replace-link-keep-label))))
   (make-org-roam-doctor-checker
    :name 'org-roam-doctor-check-roam-props
    :description "Check #+roam_* properties.")
   (make-org-roam-doctor-checker
    :name 'org-roam-doctor-check-tags
    :description "Check #+roam_tags.")
   (make-org-roam-doctor-checker
    :name 'org-roam-doctor-check-alias
    :description "Check #+roam_alias.")))

(defconst org-roam-doctor--supported-roam-properties
  '("roam_tags" "roam_alias" "roam_key")
  "List of supported Org-roam properties.")

(defun org-roam-doctor-check-roam-props (ast)
  "Checker for detecting invalid #+roam_* properties.
AST is the org-element parse tree."
  (let (reports)
    (org-element-map ast 'keyword
      (lambda (kw)
        (let ((key (org-element-property :key kw)))
          (when (and (string-prefix-p "ROAM_" key t)
                     (not (member (downcase key) org-roam-doctor--supported-roam-properties)))
            (push
             `(,(org-element-property :begin kw)
               ,(concat "Possible mispelled key: "
                        (prin1-to-string key)
                        "\nOrg-roam supports the following keys: "
                        (s-join ", " org-roam-doctor--supported-roam-properties)))
             reports)))))
    reports))

(defun org-roam-doctor-check-tags (ast)
  "Checker for detecting invalid #+roam_tags.
AST is the org-element parse tree."
  (let (reports)
    (org-element-map ast 'keyword
      (lambda (kw)
        (when (string-collate-equalp (org-element-property :key kw) "roam_tags" nil t)
          (let ((tags (org-element-property :value kw)))
            (condition-case nil
                (split-string-and-unquote tags)
              (error
               (push
                `(,(org-element-property :begin kw)
                  ,(concat "Unable to parse tags: "
                           tags
                           (when (s-contains? "," tags)
                             "\nCheck that your tags are not comma-separated.")))
                reports)))))))
    reports))

(defun org-roam-doctor-check-alias (ast)
  "Checker for detecting invalid #+roam_alias.
AST is the org-element parse tree."
  (let (reports)
    (org-element-map ast 'keyword
      (lambda (kw)
        (when (string-collate-equalp (org-element-property :key kw) "roam_alias" nil t)
          (let ((aliases (org-element-property :value kw)))
            (condition-case nil
              (split-string-and-unquote aliases)
              (error
               (push
                `(,(org-element-property :begin kw)
                  ,(concat "Unable to parse aliases: "
                           aliases
                           (when (s-contains? "," aliases)
                             "\nCheck that your aliases are not comma-separated.")))
                reports)))))))
    reports))

(defun org-roam-doctor-broken-links (ast)
  "Checker for detecting broken links.
AST is the org-element parse tree."
  (let (reports)
    (org-element-map ast 'link
      (lambda (l)
        (when (equal "file" (org-element-property :type l))
          (let ((file (org-element-property :path l)))
            (or (file-exists-p file)
                (file-remote-p file)
                (push
                 `(,(org-element-property :begin l)
                   ,(format (if (org-element-lineage l '(link))
                                "Link to non-existent image file \"%s\"\
 in link description"
                              "Link to non-existent local file \"%s\"")
                            file))
                 reports))))))
    reports))

(defun org-roam-doctor--check (buffer checkers)
  "Check BUFFER for errors.
CHECKERS is the list of checkers used."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((ast (org-element-parse-buffer))
             (errors (sort (cl-mapcan
                            (lambda (c)
                              (mapcar
                               (lambda (report)
                                 (list (set-marker (make-marker) (car report))
                                       (nth 1 report) c))
                               (save-excursion
                                 (funcall
                                  (org-roam-doctor-checker-name c)
                                  ast))))
                            checkers)
                           #'car-less-than-car)))
        (dolist (e errors)
          (pcase-let ((`(,m ,msg ,checker) e))
            (switch-to-buffer buffer)
            (goto-char m)
            (org-reveal)
            (undo-boundary)
            (org-roam-doctor--resolve msg checker)
            (set-marker m nil)))
        errors))))

;;; Actions
(defun org-roam-doctor--recursive-edit ()
  "Launch into a recursive edit."
  (message "When you're done editing press C-M-c to continue.")
  (recursive-edit))

(defun org-roam-doctor--skip ()
  "Skip the current error."
  (org-roam-message "Skipping..."))

(defun org-roam-doctor--replace-link ()
  "Replace the current link with a new link."
  (save-match-data
    (unless (org-in-regexp org-link-bracket-re 1)
      (user-error "No link at point"))
    (let ((orig (buffer-string))
          (p (point)))
      (condition-case nil
          (save-excursion
            (replace-match "")
            (org-roam-insert))
        (quit (progn
                (replace-buffer-contents orig)
                (goto-char p)))))))

(defun org-roam-doctor--replace-link-keep-label ()
  "Replace the current link with a new link, keeping the current link's label."
  (save-match-data
    (unless (org-in-regexp org-link-bracket-re 1)
      (user-error "No link at point"))
    (let ((orig (buffer-string))
          (p (point)))
      (condition-case nil
          (save-excursion
            (let ((label (if (match-end 2)
                             (match-string-no-properties 2)
                           (org-link-unescape (match-string-no-properties 1)))))
              (replace-match "")
              (org-roam-insert nil nil label)))
        (quit (progn
                (replace-buffer-contents orig)
                (goto-char p)))))))

(defun org-roam-doctor--remove-link ()
  "Unlink the text at point."
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))

(defun org-roam-doctor--resolve (msg checker)
  "Resolve an error.
MSG is the error that was found, which is displayed in a help buffer.
CHECKER is a org-roam-doctor checker instance."
  (let ((actions (org-roam-doctor-checker-actions checker))
        c)
    (push '("e" . ("Edit" . org-roam-doctor--recursive-edit)) actions)
    (push '("s" . ("Skip" . org-roam-doctor--skip)) actions)
    (with-output-to-temp-buffer "*Org-roam-doctor Help*"
      (mapc #'princ
            (list "Error message:\n   " msg "\n\n"))
      (dolist (action actions)
        (princ (format "[%s]: %s\n"
                       (car action)
                       (cadr action))))
      (princ "\n\n"))
    (shrink-window-if-larger-than-buffer
     (get-buffer-window "*Org-roam-doctor Help*"))
    (message "Press key for command:")
    (unwind-protect
        (progn
          (cl-loop
           do (setq c (char-to-string (read-char-exclusive)))
           until (assoc c actions)
           do (message "Please enter a valid key for command:"))
          (funcall (cddr (assoc c actions)))
          (redisplay))
      (when (get-buffer-window "*Org-roam-doctor Help*")
        (delete-window (get-buffer-window "*Org-roam-doctor Help*"))
        (kill-buffer "*Org-roam-doctor Help*")))))

;;;###autoload
(defun org-roam-doctor (&optional checkall)
  "Perform a check on the current buffer to ensure cleanliness.
If CHECKALL, run the check for all Org-roam files."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (let ((files (if checkall
                  (org-roam--list-all-files)
                (unless (org-roam--org-roam-file-p)
                  (user-error "Not in an org-roam file"))
                `(,(buffer-file-name)))))
    (org-roam-doctor-start files org-roam-doctor--checkers)))

(defun org-roam-doctor-start (files checkers)
  "Lint FILES using CHECKERS."
  (save-window-excursion
    (let ((existing-buffers (org-roam--get-roam-buffers))
          (org-inhibit-startup org-roam-doctor-inhibit-startup))
      (dolist (f files)
        (let ((buf (find-file-noselect f)))
          (org-roam-doctor--check buf checkers)
          (unless (memq buf existing-buffers)
            (save-buffer buf)
            (kill-buffer buf))))))
  (org-roam-message "Linting completed."))

(provide 'org-roam-doctor)

;;; org-roam-doctor.el ends here
