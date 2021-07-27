;;; org-roam-utils.el --- Utilities for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library implements utility functions used throughout
;; Org-roam.
;;
;;
;;; Code:
;;;; Library Requires
(require 'dash)

(eval-when-compile
  (require 'org-roam-macs)
  (require 'org-macs))

(defvar org-roam-verbose)

;; This is necessary to ensure all dependents on this module see
;; `org-mode-hook' and `org-inhibit-startup' as dynamic variables,
;; regardless of whether Org is loaded before their compilation.
(require 'org)

;;;; String Utilities
(defun org-roam-truncate (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

(defun org-roam-replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun org-roam-quote-string (s)
  "Quotes string S."
  (->> s
    (org-roam-replace "\\" "\\\\")
    (org-roam-replace "\"" "\\\"")))

;;;; Utility Functions
(defun org-roam--list-interleave (lst separator)
  "Interleaves elements in LST with SEPARATOR."
  (when lst
    (let ((new-lst (list (pop lst))))
      (dolist (it lst)
        (nconc new-lst (list separator it)))
      new-lst)))

(defun org-roam-up-heading-or-point-min ()
  "Fixed version of Org's `org-up-heading-or-point-min'."
  (ignore-errors (org-back-to-heading t))
  (let ((p (point)))
    (if (< 1 (funcall outline-level))
        (progn
          (org-up-heading-safe)
          (when (= (point) p)
            (goto-char (point-min))))
      (unless (bobp) (goto-char (point-min))))))

(defun org-roam-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `org-roam-verbose' is t."
  (when org-roam-verbose
    (apply #'message `(,(concat "(org-roam) " format-string) ,@args))))

(defvar org-ref-buffer-hacked)

(defun org-roam-fontify-like-in-org-mode (s)
  "Fontify string S like in Org mode.
Like `org-fontify-like-in-org-mode', but supports `org-ref'."
  ;; NOTE: pretend that the temporary buffer created by `org-fontify-like-in-org-mode' to
  ;; fontify a `cite:' reference has been hacked by org-ref, whatever that means;
  ;;
  ;; `org-ref-cite-link-face-fn', which is used to supply a face for `cite:' links, calls
  ;; `hack-dir-local-variables' rationalizing that `bibtex-completion' would throw some warnings
  ;; otherwise.  This doesn't seem to be the case and calling this function just before
  ;; `org-font-lock-ensure' (alias of `font-lock-ensure') actually instead of fixing the alleged
  ;; warnings messes the things so badly that `font-lock-ensure' crashes with error and doesn't let
  ;; org-roam to proceed further. I don't know what's happening there exactly but disabling this hackery
  ;; fixes the crashing.  Fortunately, org-ref provides the `org-ref-buffer-hacked' switch, which we use
  ;; here to make it believe that the buffer was hacked.
  ;;
  ;; This is a workaround for `cite:' links and does not have any effect on other ref types.
  ;;
  ;; `org-ref-buffer-hacked' is a buffer-local variable, therefore we inline
  ;; `org-fontify-like-in-org-mode' here
  (with-temp-buffer
    (insert s)
    (let ((org-ref-buffer-hacked t))
      (org-mode)
      (org-font-lock-ensure)
      (buffer-string))))

(defun org-roam-set-header-line-format (string)
  "Set the header-line using STRING.
If the `face' property of any part of STRING is already set, then
that takes precedence. Also pad the left side of STRING so that
it aligns with the text area."
  (setq-local header-line-format
              (concat (propertize " " 'display '(space :align-to 0))
                      string)))

;;; Keywords
(defun org-roam--get-keyword (name &optional bound)
  "Return keyword property NAME in current buffer.
If BOUND, scan up to BOUND bytes of the buffer."
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun org-roam-get-keyword (name &optional file bound)
  "Return keyword property NAME from an org FILE.
FILE defaults to current file.
Only scans up to BOUND bytes of the document."
  (unless bound
    (setq bound 1024))
  (if file
      (with-temp-buffer
        (insert-file-contents file nil 0 bound)
        (org-roam--get-keyword name))
    (org-roam--get-keyword name bound)))

;;; Shielding regions
(defface org-roam-shielded
  '((t :inherit (warning)))
  "Face for regions that are shielded (marked as read-only).
This face is used on the region target by org-roam-insertion
during an `org-roam-capture'."
  :group 'org-roam-faces)

(defun org-roam-shield-region (beg end)
  "Shield region against modifications.
BEG and END are markers for the beginning and end regions.
REGION must be a cons-cell containing the marker to the region
beginning and maximum values."
  (add-text-properties beg end
                       '(font-lock-face org-roam-shielded
                                        read-only t)
                       (marker-buffer beg)))

(defun org-roam-unshield-region (beg end)
  "Unshield the shielded REGION.
BEG and END are markers for the beginning and end regions."
  (let ((inhibit-read-only t))
    (remove-text-properties beg end
                            '(font-lock-face org-roam-shielded
                                             read-only t)
                            (marker-buffer beg))))

;;; Formatting
(defun org-roam-format (template replacer)
  "Format TEMPLATE with the function REPLACER.
REPLACER takes an argument of the format variable and optionally
an extra argument which is the EXTRA value from the call to
`org-roam-format'.
Adapted from `s-format'."
  (let ((saved-match-data (match-data)))
    (unwind-protect
        (replace-regexp-in-string
         "\\${\\([^}]+\\)}"
         (lambda (md)
           (let ((var (match-string 1 md))
                 (replacer-match-data (match-data)))
             (unwind-protect
                 (let ((v (progn
                            (set-match-data saved-match-data)
                            (funcall replacer var))))
                   (if v (format "%s" v) (signal 'org-roam-format-resolve md)))
               (set-match-data replacer-match-data)))) template
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

(defvar org-roam--cached-display-format nil)

(defun org-roam--process-display-format (format)
  "Pre-calculate minimal widths needed by the FORMAT string."
  (or org-roam--cached-display-format
      (setq org-roam--cached-display-format
            (let* ((fields-width 0)
                   (string-width
                    (string-width
                     (org-roam-format
                      format
                      (lambda (field)
                        (setq fields-width
                              (+ fields-width
                                 (string-to-number
                                  (or (cadr (split-string field ":"))
                                      "")))))))))
              (cons format (+ fields-width string-width))))))

;;; for org-roam-demote-entire-buffer in org-roam-refile.el
(defun org-roam--file-keyword-get (keyword)
  "Pull a KEYWORD setting from the top of the file.

Keyword must be specified in ALL CAPS."
  (cadr (assoc keyword
               (org-collect-keywords (list keyword)))))

(defun org-roam--file-keyword-kill (keyword)
  "Erase KEYWORD setting line from the top of the file."
  (let ((case-fold-search t))
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" keyword ":") nil t)
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (delete-char 1)))))

(defun org-roam--kill-empty-buffer ()
  "If the source buffer has been emptied, kill it.

If the buffer is associated with a file, delete the file.

If the buffer is associated with an in-process capture operation, abort the operation."
  (when (eq (buffer-size) 0)
    (if (buffer-file-name)
        (delete-file (buffer-file-name)))
    (set-buffer-modified-p nil)
    (when (and org-capture-mode
               (buffer-base-buffer (current-buffer)))
      (org-capture-kill))
    (kill-buffer (current-buffer))))

;;; Diagnostics
;;;###autoload (autoload 'org-roam-version "org-roam" nil t)
(defun org-roam-version (&optional message)
  "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((version
          (with-temp-buffer
            (insert-file-contents-literally (locate-library "org-roam.el"))
            (goto-char (point-min))
            (save-match-data
              (if (re-search-forward "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                  (substring-no-properties (match-string 1))
                "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

;;;###autoload (autoload 'org-roam-diagnostics "org-roam" nil t)
(defun org-roam-diagnostics ()
  "Collect and print info for `org-roam' issues."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*org-roam diagnostics*"))
    (erase-buffer)
    (insert (propertize "Copy info below this line into issue:\n" 'face '(:weight bold)))
    (insert (format "- Emacs: %s\n" (emacs-version)))
    (insert (format "- Framework: %s\n"
                    (condition-case _
                        (completing-read "I'm using the following Emacs framework:"
                                         '("Doom" "Spacemacs" "N/A" "I don't know"))
                      (quit "N/A"))))
    (insert (format "- Org: %s\n" (org-version nil 'full)))
    (insert (format "- Org-roam: %s" (org-roam-version)))))

(provide 'org-roam-utils)
;;; org-roam-utils.el ends here
