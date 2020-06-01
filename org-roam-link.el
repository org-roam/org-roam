;;; org-roam-link.el --- Org-roam custom link -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

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
;; This library provides the custom roam link type
;;
;;; Code:
;;;; Library Requires
(require 'org)
(require 'org-element)
(require 'dash)

;; org-roam-features
(require 'org-roam-capture)
(require 'org-roam-macs)

;; Declarations
(defvar org-roam-verbose)
(defvar org-roam-directory)
(defvar org-roam-buffer--current)
(declare-function org-roam--in-buffer-p                "org-roam")
(declare-function org-roam--backlink-to-current-p      "org-roam")
(declare-function org-roam--org-roam-file-p            "org-roam")
(declare-function org-roam--org-roam-title-p           "org-roam")
(declare-function org-roam--get-file-from-title        "org-roam")
(declare-function org-roam--title-to-slug              "org-roam")
(declare-function org-roam--format-link-title          "org-roam")
(declare-function org-roam--format-link                "org-roam")
(declare-function org-roam--get-title-from-file        "org-roam")
(declare-function org-roam--get-title-path-completions "org-roam")

;;;; Customizable Variables
(defcustom org-roam-link-enabled nil
  "When t `org-roam-insert' inserts roam-link instead of org file-link."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-link-hide-brackets t
  "Whether to hide brackets around [[roam:]] links."
  :type 'boolean
  :group 'org-roam)

;;;; Dynamic variables
(defvar org-roam-link-message-timer nil
  "Set by `org-roam-link-show-messages' or `org-roam-link-cancel-messages'.")

;;;; Custom org-link, roam:
(defvar org-roam-link-auto-template-key nil
  "Template key for `org-roam-capture-templates' to use for auto file creation.
Template MUST use :immediate-finish t or only first capture will be saved.")

(defvar org-roam-link-auto-template
'(("a" "Automatic" plain (function org-roam--capture-get-point)
     ""
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+TITLE: ${title}\n#+AUTOMATICALLY_CREATED: %<%Y%m%d%H%M%S>\n"
     :unnarrowed t
     :immediate-finish t))
  "Default auto-template for use with `org-roam-link--auto-create-file'.
Used if `org-roam-link-auto-template-key' is nil.")

(defvar org-roam-link--re
  "\\(\\[\\[\\)\\(roam:\\).*\\(\\]\\]\\)"
  "Matches a 'roam:' link in double brackets.")

(defface org-roam-link-brackets
  '((t :inherit org-link))
  "Face for roam: link brackets."
  :group 'org-roam-faces)

(defun org-roam-link--activate (start end _path bracketp)
  "Hides roam: link prefix and determines additional font-locking.
Optionally hide brackets before/after the link, or change their face.
If link uses a Description syntax, hide link path also, and optionally
show a single bracket pair around Description.
START and END are buffer position at start/end of the link, PATH is the
link's path as a string, and BRACKETP is a boolean that is non-nil when the
link has brackets."
  (when bracketp
    (save-excursion
      (save-match-data
        (goto-char start)
        (re-search-forward org-roam-link--re end t)
        ;; Optionally hide starting brackets or change their face
        (add-text-properties
         (match-beginning 1)
         (match-end 1)
         `(face org-roam-link-brackets invisible ,org-roam-link-hide-brackets))
        (add-text-properties
         (match-beginning 3)
         (match-end 3)
         `(face org-roam-link-brackets invisible ,org-roam-link-hide-brackets))
        ;; Check if link is plain or Descriptive
        (if (not (string-match-p "\\]\\[" (buffer-substring start end)))
            ;; If plain, hide roam: prefix
            (add-text-properties
             (match-beginning 2)
             (match-end 2)
             '(invisible t))
          ;; If descriptive, hide roam: and path. Optionally show single brackets
          (progn (if org-roam-link-hide-brackets
                     (add-text-properties start end '(invisible t))
                   (add-text-properties (+ start 1) (- end 1) '(invisible t)))
                 (save-match-data
                   (goto-char start)
                   (re-search-forward org-link-bracket-re end t)
                   (add-text-properties (match-beginning 2) (match-end 2) '(invisible nil)))))))))

(defun org-roam-link--backlink-to-current-p (path)
  "Return t if roam-link backlink is to the current Org-roam file.
PATH is a potential TITLE/ALIAS of an existing Org-roam note."
  (let ((current (buffer-file-name org-roam-buffer--current))
        (backlink-dest (org-roam--get-file-from-title path)))
    (string= current backlink-dest)))

(defun org-roam-link--face (path)
  "Conditional face for custom roam-links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer,
`org-roam-link' if PATH corresponds to any other Org-roam
TITLE/ALIAS in the Org-roam database, or `org-roam-link-invalid'
otherwise."
  (cond ((and (org-roam--in-buffer-p)
              (org-roam-link--backlink-to-current-p path))
         'org-roam-link-current)
        ((org-roam--org-roam-title-p path)
         'org-roam-link)
        (t
         'org-roam-link-invalid)))

(defun org-roam-link--find-file (title)
  "Find and open an Org-roam file based on its TITLE/ALIAS.
TITLE is TITLE/ALIAS of potential Org-roam note.
If TITLE doesn't match an existing note, prompt Org-roam
note creation using `org-roam-capture--capture'"
  (let* ((file-path (org-roam--get-file-from-title title)))
    (if file-path
        (find-file file-path)
      (if (org-roam-capture--in-process-p)
          (user-error "Nested Org-roam capture processes not supported")
        (let ((org-roam-capture--info `((title . ,title)
                                        (slug  . ,(org-roam--title-to-slug title))))
              (org-roam-capture--context 'title))
          (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
          (org-roam-capture--capture))))))

(defun org-roam-link--convert-roam-to-file-link (element)
  "Convert a custom roam-link to standard org file-link.
ELEMENT is the `org-element-at-point'.
Ignore roam-links that do not point to an existing Org-roam file."
  (let* ((path (org-element-property :path element))
         (filename (org-roam--get-file-from-title path)))
    (when filename
      (let* ((start (org-element-property :begin element))
             (end (org-element-property :end element))
             (contents-begin (org-element-property :contents-begin element))
             (contents-end (org-element-property :contents-end element))
             (desc (and contents-begin contents-end
                        (buffer-substring-no-properties contents-begin contents-end)))
             (link-desc (if desc
                            desc
                          (org-roam--format-link-title path))))
        (setf (buffer-substring start end)
              (concat (org-roam--format-link filename link-desc)
                      (make-string (org-element-property :post-blank element) ?\s)))))))

(defun org-roam-link--convert-file-to-roam-link (element)
  "Convert a standard org file-link to a custom roam-link.
ELEMENT is the `org-element-at-point'.
Ignore file-links that are not in Org-roam database."
  (let ((path (org-element-property :path element)))
    (when (org-roam--org-roam-file-p path)
      (let* ((start (org-element-property :begin element))
             (end (org-element-property :end element))
             (contents-begin (org-element-property :contents-begin element))
             (contents-end (org-element-property :contents-end element))
             (desc (and contents-begin contents-end
                        (buffer-substring-no-properties contents-begin contents-end)))
             (list-titles (caar (org-roam--get-title-from-file path)))
             (-compare-fn #'cl-equalp)
             (title (if (-contains? list-titles desc)
                            desc
                      (car list-titles)))
             (link-desc (if (string= title desc)
                            nil
                          desc)))
        (when title
          (setf (buffer-substring start end)
              (concat (org-link-make-string (concat "roam:" title) link-desc)
                      (make-string (org-element-property :post-blank element) ?\s))))))))

(defun org-roam-link-convert-roam-to-file-link ()
  "Convert a custom roam-link to standard org file-link."
  (interactive)
  (let ((elem (org-element-context)))
    (when (and (eq 'link (car elem))
               (string= "roam" (org-element-property :type elem)))
      (org-roam-link--convert-roam-to-file-link elem))))

(defun org-roam-link-convert-file-to-roam-link ()
  "Convert a standard org file-link to custom roam-link."
  (interactive)
  (let ((elem (org-element-context)))
    (when (and (eq 'link (car elem))
               (string= "file" (org-element-property :type elem)))
      (org-roam-link--convert-file-to-roam-link elem))))

(defun org-roam-link-convert-buffer-links (file-or-roam)
  "Convert all Org-roam links in the current buffer.
FILE-OR-ROAM should be one of `file' or `roam', and
indicate the link-type the buffer should convert to."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Convert to link type: " '("file" "roam") nil t))))
  (save-excursion
    (goto-char (point-min))
    (org-next-link)
    (while (not org-link--search-failed)
      (let* ((elem (org-element-context))
             (type (org-element-property :type elem)))
        (if (and (string= file-or-roam "roam")
                 (string= type "file"))
            (org-roam-link--convert-file-to-roam-link elem)
          (when (and (string= file-or-roam "file")
                     (string= type "roam"))
            (org-roam-link--convert-roam-to-file-link elem)))
        (org-next-link)))))

(defun org-roam-link--auto-create-file (title &optional manual)
  "Call `org-roam-capture--capture' with `org-roam-link-auto-template'.
If `org-roam-link-auto-template-key' is non-nil, use custom template key with
`org-roam-capture-templates' instead.
TITLE is the title for the file to be created.
If MANUAL is non-nil, allow manual selection of capture template."
  (let ((org-roam-capture--info `((title . ,title)
                                  (slug  . ,(org-roam--title-to-slug title))))
        (org-roam-capture--context 'title))
    (cond (manual (org-roam-capture--capture))
          (org-roam-link-auto-template-key
           (org-roam-capture--capture :keys org-roam-link-auto-template-key))
          (t
           (let ((org-roam-capture-templates org-roam-link-auto-template))
             (org-roam-capture--capture :keys (caar org-roam-link-auto-template)))))))

(defun org-roam-link-auto-create-links-in-buffer (&optional manual)
  "Create all non-existent roam-link files in current buffer.
If MANUAL is non-nil, prompt for template with `org-roam-capture--capture'.
Templates MUST use :immediate-finish t, or only the first non-immediate capture
will be saved correctly."
  (interactive "P")
  (when (org-roam--org-roam-file-p)
    (save-excursion
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let* ((type (org-element-property :type link))
                 (path (org-element-property :path link))
                 (roam-file (if (string= type "roam") (org-roam--get-file-from-title path) t)))
            (unless roam-file
              (let* ((start (org-element-property :begin link))
                     (end (org-element-property :end link))
                     (ov (make-overlay start end)))
                (progn
                  (goto-char end)
                  (recenter)
                  (overlay-put ov 'face 'highlight)
                  (org-roam-link--auto-create-file path manual)
                  (delete-overlay ov))))))))))

(defun org-roam-link--current-buffer-roam-link-titles ()
  "Return a list of unique roam-link titles in the current buffer."
  (->> (org-element-map (org-element-parse-buffer) 'link
         (lambda (link)
           (let* ((type (org-element-property :type link))
                  (path (org-element-property :path link))
                  res)
             (when (string= type "roam")
               (cons path res)))))
       (-flatten)
       (-distinct)))

(defun org-roam-link--completion (&optional _)
  "Completion for roam-links in `org-mode'."
  (let ((completions (org-roam--get-title-path-completions))
        (in-buffer-completions (org-roam-link--current-buffer-roam-link-titles)))
    (format "roam:%s" (completing-read "Roam note: "
                                       (-union (map-keys completions) in-buffer-completions)))))

(defun org-roam-link--status-message ()
  "Print roam-link status message to minibuffer."
  (when (and org-roam-verbose
             (string= major-mode "org-mode"))
    (let ((elem (org-element-context)))
      (when (and (eq (car elem) 'link)
                 (string= "roam" (org-element-property :type elem)))
        (save-match-data
          (let* ((title (org-element-property :path elem))
                 (abs-file-path (org-roam--get-file-from-title title))
                 (rel-file-path
                  (when abs-file-path
                    (string-match (format "%s\\(.*\\)" org-roam-directory) abs-file-path)))
                 (file-path (when rel-file-path
                              (concat "~org-roam-dir~" (match-string 1 abs-file-path))))
                 (raw-link (org-element-property :raw-link elem)))
            (if file-path
                (org-roam-message "file: %s → %s" file-path raw-link)
              (org-roam-message "No file found in db → %s" raw-link))))))))

(defun org-roam-link-show-messages ()
  "Enable minibuffer status message for roam-links.
Follows example of `org-ref' and displays on idle timer."
  (interactive)
  (when org-roam-verbose
    (or org-roam-link-message-timer
        (setq org-roam-link-message-timer
              (run-with-idle-timer 0.5 t #'org-roam-link--status-message)))))

(defun org-roam-link-cancel-messages ()
  "Disable minibuffer status message for roam-links."
  (interactive)
  (cancel-timer org-roam-link-message-timer)
  (setq org-roam-link-message-timer nil))

(defun org-roam-link-insert (&optional title)
  "Insert a roam-link with TITLE.
If TITLE is nil, prompt for one."
  (interactive)
  (if title
      (insert "[[" (concat "roam:" title) "]]")
    (insert "[[" (org-roam-link--completion) "]]")))

(provide 'org-roam-link)

;;; org-roam-link.el ends here
