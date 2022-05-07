;;; org-roam-dailies.el --- Daily-notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;      Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org-roam "2.1"))

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
;; This extension provides functionality for creating daily-notes, or shortly
;; "dailies". Dailies implemented here as a unique node per unique file, where
;; each file named after certain date and stored in `org-roam-dailies-directory'.
;;
;; One can use dailies for various purposes, e.g. journaling, fleeting notes,
;; scratch notes or whatever else you can think of.
;;
;;; Code:
(require 'dash)
(require 'org-roam)

;;; Faces
(defface org-roam-dailies-calendar-note
  '((t :inherit (org-link) :underline nil))
  "Face for dates with a daily-note in the calendar."
  :group 'org-roam-faces)

;;; Options
(defcustom org-roam-dailies-directory "daily/"
  "Path to daily-notes.
This path is relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-dailies-find-file-hook nil
  "Hook that is run right after navigating to a daily-note."
  :group 'org-roam
  :type 'hook)

(defcustom org-roam-dailies-capture-templates
  `(("d" "default" entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n")))
  "Capture templates for daily-notes in Org-roam.
Note that for daily files to show up in the calendar, they have to be of format
\"org-time-string.org\".
See `org-roam-capture-templates' for the template documentation."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :target)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

;;; Commands
;;;; Today
;;;###autoload
(defun org-roam-dailies-capture-today (&optional goto keys)
  "Create an entry in the daily-note for today.
When GOTO is non-nil, go the note without creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "P")
  (org-roam-dailies--capture (current-time) goto keys))

;;;###autoload
(defun org-roam-dailies-goto-today (&optional keys)
  "Find the daily-note for today, creating it if necessary.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive)
  (org-roam-dailies-capture-today t keys))

;;;; Tomorrow
;;;###autoload
(defun org-roam-dailies-capture-tomorrow (n &optional goto keys)
  "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "p")
  (org-roam-dailies--capture (time-add (* n 86400) (current-time)) goto keys))

;;;###autoload
(defun org-roam-dailies-goto-tomorrow (n &optional keys)
  "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow n t keys))

;;;; Yesterday
;;;###autoload
(defun org-roam-dailies-capture-yesterday (n &optional goto keys)
  "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) goto keys))

;;;###autoload
(defun org-roam-dailies-goto-yesterday (n &optional keys)
  "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) t keys))

;;;; Date
;;;###autoload
(defun org-roam-dailies-capture-date (&optional goto prefer-future keys)
  "Create an entry in the daily-note for a date using the calendar.
Prefer past dates, unless PREFER-FUTURE is non-nil.
With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date nil t nil (if goto
                                             "Find daily-note: "
                                           "Capture to daily-note: ")))))
    (org-roam-dailies--capture time goto keys)))

;;;###autoload
(defun org-roam-dailies-goto-date (&optional prefer-future keys)
  "Find the daily-note for a date using the calendar, creating it if necessary.
Prefer past dates, unless PREFER-FUTURE is non-nil.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive)
  (org-roam-dailies-capture-date t prefer-future keys))

;;;; Navigation
(defun org-roam-dailies-goto-next-note (&optional n)
  "Find next daily-note.

With numeric argument N, find note N days in the future. If N is
negative, find note N days in the past."
  (interactive "p")
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (setq n (or n 1))
  (let* ((dailies (org-roam-dailies--list-files))
         (position
          (cl-position-if (lambda (candidate)
                            (string= (buffer-file-name (buffer-base-buffer)) candidate))
                          dailies))
         note)
    (unless position
      (user-error "Can't find current note file - have you saved it yet?"))
    (pcase n
      ((pred (natnump))
       (when (eq position (- (length dailies) 1))
         (user-error "Already at newest note")))
      ((pred (integerp))
       (when (eq position 0)
         (user-error "Already at oldest note"))))
    (setq note (nth (+ position n) dailies))
    (find-file note)
    (run-hooks 'org-roam-dailies-find-file-hook)))

(defun org-roam-dailies-goto-previous-note (&optional n)
  "Find previous daily-note.

With numeric argument N, find note N days in the past. If N is
negative, find note N days in the future."
  (interactive "p")
  (let ((n (if n (- n) -1)))
    (org-roam-dailies-goto-next-note n)))

(defun org-roam-dailies--list-files (&rest extra-files)
  "List all files in `org-roam-dailies-directory'.
EXTRA-FILES can be used to append extra files to the list."
  (let ((dir (expand-file-name org-roam-dailies-directory org-roam-directory))
        (regexp (rx-to-string `(and "." (or ,@org-roam-file-extensions)))))
    (append (--remove (let ((file (file-name-nondirectory it)))
                        (when (or (auto-save-file-name-p file)
                                  (backup-file-name-p file)
                                  (string-match "^\\." file))
                          it))
                      (directory-files-recursively dir regexp))
            extra-files)))

(defun org-roam-dailies--daily-note-p (&optional file)
  "Return t if FILE is an Org-roam daily-note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (expand-file-name
                    (or file
                        (buffer-file-name (buffer-base-buffer)))))
             (directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
    (setq path (expand-file-name path))
    (save-match-data
      (and
       (org-roam-file-p path)
       (org-roam-descendant-of-p path directory)))))

;;;###autoload
(defun org-roam-dailies-find-directory ()
  "Find and open `org-roam-dailies-directory'."
  (interactive)
  (find-file (expand-file-name org-roam-dailies-directory org-roam-directory)))

;;; Calendar integration
(defun org-roam-dailies-calendar--file-to-date (file)
  "Convert FILE to date.
Return (MONTH DAY YEAR) or nil if not an Org time-string."
  (ignore-errors
    (cl-destructuring-bind (_ _ _ d m y _ _ _)
        (org-parse-time-string
         (file-name-sans-extension
          (file-name-nondirectory file)))
      (list m d y))))

(defun org-roam-dailies-calendar-mark-entries ()
  "Mark days in the calendar for which a daily-note is present."
  (when (file-exists-p (expand-file-name org-roam-dailies-directory org-roam-directory))
    (dolist (date (remove nil
                          (mapcar #'org-roam-dailies-calendar--file-to-date
                                  (org-roam-dailies--list-files))))
      (when (calendar-date-is-visible-p date)
        (calendar-mark-visible-date date 'org-roam-dailies-calendar-note)))))

(add-hook 'calendar-today-visible-hook #'org-roam-dailies-calendar-mark-entries)
(add-hook 'calendar-today-invisible-hook #'org-roam-dailies-calendar-mark-entries)

;;; Capture implementation
(add-to-list 'org-roam-capture--template-keywords :override-default-time)

(defun org-roam-dailies--capture (time &optional goto keys)
  "Capture an entry in a daily-note for TIME, creating it if necessary.
When GOTO is non-nil, go the note without creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
        (org-roam-dailies-directory "./"))
    (org-roam-capture- :goto (when goto '(4))
                       :keys keys
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time time)))
  (when goto (run-hooks 'org-roam-dailies-find-file-hook)))

(add-hook 'org-roam-capture-preface-hook #'org-roam-dailies--override-capture-time-h)
(defun org-roam-dailies--override-capture-time-h ()
  "Override the `:default-time' with the time from `:override-default-time'."
  (prog1 nil
    (when (org-roam-capture--get :override-default-time)
      (org-capture-put :default-time (org-roam-capture--get :override-default-time)))))

;;; Bindings
(defvar org-roam-dailies-map (make-sparse-keymap)
  "Keymap for `org-roam-dailies'.")

(define-prefix-command 'org-roam-dailies-map)

(define-key org-roam-dailies-map (kbd "d") #'org-roam-dailies-goto-today)
(define-key org-roam-dailies-map (kbd "y") #'org-roam-dailies-goto-yesterday)
(define-key org-roam-dailies-map (kbd "t") #'org-roam-dailies-goto-tomorrow)
(define-key org-roam-dailies-map (kbd "n") #'org-roam-dailies-capture-today)
(define-key org-roam-dailies-map (kbd "f") #'org-roam-dailies-goto-next-note)
(define-key org-roam-dailies-map (kbd "b") #'org-roam-dailies-goto-previous-note)
(define-key org-roam-dailies-map (kbd "c") #'org-roam-dailies-goto-date)
(define-key org-roam-dailies-map (kbd "v") #'org-roam-dailies-capture-date)
(define-key org-roam-dailies-map (kbd ".") #'org-roam-dailies-find-directory)

(provide 'org-roam-dailies)

;;; org-roam-dailies.el ends here
