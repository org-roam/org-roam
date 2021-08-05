;;; org-roam-utils.el --- Utilities for Org-roam -*- lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.4"))

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
;; This library provides definitions for utilities that used throughout the
;; whole package.
;;
;;; Code:
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes

;;; String utilities
(defun org-roam-truncate-string (s len &optional ellipsis)
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

;; TODO Refactor this.
(defun org-roam-replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun org-roam-quote-string (s)
  "Quotes string S."
  (->> s
    (org-roam-replace "\\" "\\\\")
    (org-roam-replace "\"" "\\\"")))

;;; List utilities
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

;;; File utilities
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

(defun org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let* ((path (or file (buffer-file-name (buffer-base-buffer))))
         (ext (when path (org-roam--file-name-extension path)))
         (ext (if (string= ext "gpg")
                  (org-roam--file-name-extension (file-name-sans-extension path))
                ext)))
    (save-match-data
      (and
       path
       (member ext org-roam-file-extensions)
       (not (and org-roam-file-exclude-regexp
                 (string-match-p org-roam-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-directory))))))

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (--> cmd
       (shell-command-to-string it)
       (ansi-color-filter-apply it)
       (split-string it "\n")
       (seq-filter #'s-present? it)))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (cl-loop for e in exts
           append (list (format "\"*.%s\"" e)
                        (format "\"*.%s.gpg\"" e))))

(defun org-roam--list-files-fd (executable dir)
  "Return all Org-roam files located recursively within DIR, using fd, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (extensions (s-join " -e " (mapcar (lambda (glob) (substring glob 2 -1)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "--type file" ,extensions))))
    (org-roam--shell-command-files command)))

(defalias 'org-roam--list-files-fdfind #'org-roam--list-files-fd)

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (names (s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\(" ,names "\\)"))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat
                                  #'regexp-quote org-roam-file-extensions
                                  "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (org-roam--directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file)
                 (org-roam-file-p file))
        (push file result)))))

;;; Buffer utilities
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

(defun org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of Org-roam system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam-file-p path)))))

(defun org-roam-buffer-list ()
  "Return a list of buffers that are Org-roam files."
  (--filter (org-roam-buffer-p it)
            (buffer-list)))

;;; Formatting
(defun org-roam-format (template replacer)
  "Format TEMPLATE with the function REPLACER.
The templates are of form ${foo} for variable foo, and
${foo=default} for variable foo with default value \"default\".
REPLACER takes an argument of the format variable and the default
value (possibly nil). Adapted from `s-format'."
  (let ((saved-match-data (match-data)))
    (unwind-protect
        (replace-regexp-in-string
         "\\${\\([^}]+\\)}"
         (lambda (md)
           (let ((var (match-string 1 md))
                 (replacer-match-data (match-data))
                 default-val)
             (when (string-match "\\(.+\\)=\\(.+\\)" var)
               (setq default-val (match-string 2 var)
                     var (match-string 1 var)))
             (unwind-protect
                 (let ((v (progn
                            (set-match-data saved-match-data)
                            (funcall replacer var default-val))))
                   (if v (format "%s" v) (signal 'org-roam-format-resolve md)))
               (set-match-data replacer-match-data)))) template
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

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

;;; Org-mode utilities
;;;; Motions
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

;;;; Keywords
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

(defun org-roam--get-keyword (name &optional bound)
  "Return keyword property NAME in current buffer.
If BOUND, scan up to BOUND bytes of the buffer."
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun org-roam-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" key ": " value "\n")))))

(defun org-roam-erase-keyword (keyword)
  "Erase the line where the KEYWORD is, setting line from the top of the file."
  (let ((case-fold-search t))
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" keyword ":") nil t)
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (delete-char 1)))))

;;;; Properties
(defun org-roam-add-property (val prop)
  "Add VAL value to PROP property for the node at point.
Both, VAL and PROP are strings."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq val lst) lst (cons val lst)))
         (lst (seq-uniq lst)))
    (org-set-property prop (combine-and-quote-strings lst))
    val))

(defun org-roam-remove-property (prop &optional val)
  "Remove VAL value from PROP property for the node at point.
Both VAL and PROP are strings.

If VAL is not specified, user is prompted to select a value."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (prop-to-remove (or val (completing-read "Remove: " lst)))
         (lst (delete prop-to-remove lst)))
    (if lst
        (org-set-property prop (combine-and-quote-strings lst))
      (org-delete-property prop))
    prop-to-remove))

;;; Logs
(defvar org-roam-verbose)
(defun org-roam-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `org-roam-verbose' is t."
  (when org-roam-verbose
    (apply #'message `(,(concat "(org-roam) " format-string) ,@args))))

;;; Diagnostics
;; TODO Update this to also get commit hash
;;;###autoload
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

;;;###autoload
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
