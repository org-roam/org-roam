;;; org-roam-dailies.el --- Daily-notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-
;;;
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; 	Leo Vivier <leo.vivier+dev@gmail.com>
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
;; This library provides functionality for creating daily-notes. This is a
;; concept borrowed from Roam Research.
;;
;;; Code:
;;; Library Requires
(require 'org-capture)
(require 'org-roam-capture)
(require 'org-roam-macs)
(require 'f)

;;;; Declarations
(defvar org-roam-mode)
(defvar org-roam-directory)
(defvar org-roam-file-extensions)
(declare-function org-roam--org-file-p        "org-roam")
(declare-function org-roam--find-file         "org-roam")
(declare-function org-roam-mode               "org-roam")

;;;; Customizable variables
(defcustom org-roam-dailies-directory "daily/"
  "Path to daily-notes."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-dailies-find-file-hook nil
  "Hook that is run right after navigating to a daily-note."
  :group 'org-roam
  :type 'hook)

(defcustom org-roam-dailies-capture-templates
  `(("d" "default" entry (function org-roam-capture--get-point)
     "* %?"
     :file-name ,(concat org-roam-dailies-directory "%<%Y-%m-%d>")
     :head "#+title: %<%Y-%m-%d>\n"))
  "Capture templates for daily-notes in Org-roam."
  :group 'org-roam
  ;; Adapted from `org-capture-templates'
  :type
  `(repeat
    (choice :value ("d" "default" plain (function org-roam-capture--get-point)
                    "%?"
                    :file-name ,(concat org-roam-dailies-directory
					"%<%Y-%m-%d>")
                    :head "#+title: %<%Y-%m-%d>\n"
                    :unnarrowed t)
      (list :tag "Multikey description"
        (string :tag "Keys       ")
        (string :tag "Description"))
      (list :tag "Template entry"
        (string :tag "Keys              ")
        (string :tag "Description       ")
        (choice :tag "Type              "
         (const :tag "Plain" plain)
         (const :tag "Entry (for creating headlines)" entry))
        (const :format "" #'org-roam-capture--get-point)
        (choice :tag "Template          "
          (string :tag "String"
                  :format "String:\n            \
Template string   :\n%v")
          (list :tag "File"
            (const :format "" file)
            (file :tag "Template file     "))
          (list :tag "Function"
            (const :format "" function)
            (function :tag "Template function ")))
        (const :format "File name format  :" :file-name)
        (string :format " %v" :value ,(concat org-roam-dailies-directory
					      "%<%Y-%m-%d>"))
        (const :format "Header format     :" :head)
        (string :format " %v" :value "#+title: ${title}\n")
        (plist :inline t
               :tag "Options"
          ;; Give the most common options as checkboxes
               :options
               (((const :tag "Outline path" :olp)
                 (repeat :tag "Headings"
                   (string :tag "Heading")))
                ((const :format "%v " :unnarrowed) (const t))
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
                ((const :format "%v " :table-line-pos) (string))
                ((const :format "%v " :kill-buffer) (const t))))))))

;;;; Utilities
(defun org-roam-dailies-directory--get-absolute-path ()
  "Get absolute path to `org-roam-dailies-directory'."
  (expand-file-name org-roam-dailies-directory org-roam-directory))

(defun org-roam-dailies-find-directory ()
  "Find and open `org-roam-dailies-directory'."
  (interactive)
  (org-roam--find-file (org-roam-dailies-directory--get-absolute-path)))

(defun org-roam-dailies--daily-note-p (&optional file)
  "Return t if FILE is an Org-roam daily-note, nil otherwise.

If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (or file
                       (-> (buffer-base-buffer)
                           (buffer-file-name))))
             (directory (org-roam-dailies-directory--get-absolute-path)))
    (setq path (expand-file-name path))
    (save-match-data
      (and
       (org-roam--org-file-p path)
       (f-descendant-of-p path directory)))))

(defun org-roam-dailies--capture (time &optional goto)
  "Capture an entry in a daily-note for TIME, creating it if necessary.

When GOTO is non-nil, go the note without creating an entry."
  (unless org-roam-mode (org-roam-mode))
  (let ((org-roam-capture-templates (--> org-roam-dailies-capture-templates
                                         (if goto (list (car it)) it)))
        (org-roam-capture--info (list (cons 'time time)))
        (org-roam-capture--context 'dailies))
    (org-roam-capture--capture (when goto '(4)))))

;;;; Commands
;;; Today
(defun org-roam-dailies-capture-today (&optional goto)
  "Create an entry in the daily-note for today.

When GOTO is non-nil, go the note without creating an entry."
  (interactive "P")
  (org-roam-dailies--capture (current-time) goto)
  (when goto
    (run-hooks 'org-roam-dailies-find-file-hook)))

(defun org-roam-dailies-find-today ()
  "Find the daily-note for today, creating it if necessary."
  (interactive)
  (org-roam-dailies-capture-today t))

;;; Tomorrow
(defun org-roam-dailies-capture-tomorrow (n &optional goto)
  "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry."
  (interactive "p")
  (org-roam-dailies--capture (time-add (* n 86400) (current-time)) goto))

(defun org-roam-dailies-find-tomorrow (n)
  "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow n t))

;;; Yesterday
(defun org-roam-dailies-capture-yesterday (n &optional goto)
  "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) goto))

(defun org-roam-dailies-find-yesterday (n)
  "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future."
  (interactive "p")
  (org-roam-dailies-capture-tomorrow (- n) t))

;;; Calendar
(defvar org-roam-dailies-calendar-hook (list 'org-roam-dailies-calendar-mark-entries)
  "Hooks to run when showing the `org-roam-dailies-calendar'.")

(defun org-roam-dailies-calendar--install-hook ()
  "Install Org-roam-dailies hooks to calendar."
  (add-hook 'calendar-today-visible-hook #'org-roam-dailies-calendar--run-hook)
  (add-hook 'calendar-today-invisible-hook #'org-roam-dailies-calendar--run-hook))

(defun org-roam-dailies-calendar--run-hook ()
  "Run Org-roam-dailies hooks to calendar."
  (run-hooks 'org-roam-dailies-calendar-hook)
  (remove-hook 'calendar-today-visible-hook #'org-roam-dailies-calendar--run-hook)
  (remove-hook 'calendar-today-invisible-hook #'org-roam-dailies-calendar--run-hook))

(defun org-roam-dailies-calendar--file-to-date (&optional file)
  "Convert FILE to date.

Return (MONTH DAY YEAR)."
  (let ((file (or file
                  (-> (buffer-base-buffer)
                      (buffer-file-name)))))
    (cl-destructuring-bind (_ _ _ d m y _ _ _)
        (-> file
            (file-name-nondirectory)
            (file-name-sans-extension)
            (org-parse-time-string))
      (list m d y))))

(defun org-roam-dailies-calendar--date-to-time (date)
  "Convert DATE as returned from the calendar (MONTH DAY YEAR) to a time."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun org-roam-dailies-calendar-mark-entries ()
  "Mark days in the calendar for which a daily-note is present."
  (when (file-exists-p (org-roam-dailies-directory--get-absolute-path))
    (dolist (date (mapcar #'org-roam-dailies-calendar--file-to-date
                          (org-roam-dailies--list-files)))
      (when (calendar-date-is-visible-p date)
        (calendar-mark-visible-date date 'org-roam-dailies-calendar-note)))))

;;; Date
(defun org-roam-dailies-capture-date (&optional goto prefer-future)
  "Create an entry in the daily-note for a date using the calendar.

Prefer past dates, unless PREFER-FUTURE is non-nil.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry."
  (interactive "P")
  (org-roam-dailies-calendar--install-hook)
  (let* ((time-str (let ((org-read-date-prefer-future prefer-future))
                     (org-read-date nil nil nil (if goto
                                                    "Find daily-note: "
                                                  "Capture to daily-note: "))))
         (time (org-read-date nil t time-str)))
    (org-roam-dailies--capture time goto)
    (when goto
      (run-hooks 'org-roam-dailies-find-file-hook))))

(defun org-roam-dailies-find-date (&optional prefer-future)
  "Find the daily-note for a date using the calendar, creating it if necessary.

Prefer past dates, unless PREFER-FUTURE is non-nil."
  (interactive)
  (org-roam-dailies-capture-date t prefer-future))

;;; Navigation
(defun org-roam-dailies--list-files (&rest extra-files)
  "List all files in `org-roam-dailies-directory'.
EXTRA-FILES can be used to append extra files to the list."
  (let ((dir (org-roam-dailies-directory--get-absolute-path))
	(regexp (rx-to-string `(and "." (or ,@org-roam-file-extensions)))))
    (append (--remove (let ((file (file-name-nondirectory it)))
                        (when (or (auto-save-file-name-p file)
                                  (backup-file-name-p file)
                                  (string-match "^\\." file))
                          it))
                      (directory-files-recursively dir regexp))
            extra-files)))

(defun org-roam-dailies-find-next-note (&optional n)
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

(defun org-roam-dailies-find-previous-note (&optional n)
  "Find previous daily-note.

With numeric argument N, find note N days in the past. If N is
negative, find note N days in the future."
  (interactive "p")
  (let ((n (if n (- n) -1)))
    (org-roam-dailies-find-next-note n)))

;;;; Bindings
(defvar org-roam-dailies-map (make-sparse-keymap)
  "Keymap for `org-roam-dailies'.")

(define-prefix-command 'org-roam-dailies-map)

(define-key org-roam-dailies-map (kbd "d") #'org-roam-dailies-find-today)
(define-key org-roam-dailies-map (kbd "y") #'org-roam-dailies-find-yesterday)
(define-key org-roam-dailies-map (kbd "t") #'org-roam-dailies-find-tomorrow)
(define-key org-roam-dailies-map (kbd "n") #'org-roam-dailies-capture-today)
(define-key org-roam-dailies-map (kbd "f") #'org-roam-dailies-find-next-note)
(define-key org-roam-dailies-map (kbd "b") #'org-roam-dailies-find-previous-note)
(define-key org-roam-dailies-map (kbd "c") #'org-roam-dailies-find-date)
(define-key org-roam-dailies-map (kbd "v") #'org-roam-dailies-capture-date)
(define-key org-roam-dailies-map (kbd ".") #'org-roam-dailies-find-directory)

(provide 'org-roam-dailies)

;;; org-roam-dailies.el ends here
