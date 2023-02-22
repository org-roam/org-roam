;;; org-roam-utils.el --- Utilities for Org-roam -*- lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
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

(require 'org-roam)

(defun org-roam-require (libs)
  "Require LIBS."
  (dolist (lib libs)
    (require lib nil 'noerror)))

;;; String utilities
;; TODO Refactor this.
(defun org-roam-replace-string (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun org-roam-quote-string (s)
  "Quotes string S."
  (->> s
       (org-roam-replace-string "\\" "\\\\")
       (org-roam-replace-string "\"" "\\\"")))

(defun org-roam-word-wrap (len s)
  "If S is longer than LEN, wrap the words with newlines."
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (insert s)
      (let ((fill-column len))
        (fill-region (point-min) (point-max)))
      (buffer-substring (point-min) (point-max)))))

(defun org-roam-string-equal (s1 s2)
  "Return t if S1 and S2 are equal.
Like `string-equal', but case-insensitive."
  (and (= (length s1) (length s2))
       (or (string-equal s1 s2)
           (string-equal (downcase s1) (downcase s2)))))

(defun org-roam-whitespace-content (s)
  "Return the whitespace content at the end of S."
  (with-temp-buffer
    (let ((c 0))
      (insert s)
      (skip-chars-backward " \t\n")
      (buffer-substring-no-properties
       (point) (point-max)))))

(defun org-roam-strip-comments (s)
  "Strip Org comments from string S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (not (eobp))
      (if (org-at-comment-p)
          (delete-region (point-at-bol) (progn (forward-line) (point)))
        (forward-line)))
    (buffer-string)))

;;; List utilities
(defun org-roam-plist-map! (fn plist)
  "Map FN over PLIST, modifying it in-place and returning it.
FN must take two arguments: the key and the value."
  (let ((plist-index plist))
    (while plist-index
      (let ((key (pop plist-index)))
        (setf (car plist-index) (funcall fn key (car plist-index))
              plist-index (cdr plist-index)))))
  plist)

(defmacro org-roam-dolist-with-progress (spec msg &rest body)
  "Loop over a list and report progress in the echo area.
Like `dolist-with-progress-reporter', but falls back to `dolist'
if the function does not yet exist.

Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

MSG is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

SPEC is a list, as per `dolist'."
  (declare (indent 2))
  (if (fboundp 'dolist-with-progress-reporter)
      `(dolist-with-progress-reporter ,spec ,msg ,@body)
    `(dolist ,spec ,@body)))

;;; File utilities
(defun org-roam-descendant-of-p (a b)
  "Return t if A is descendant of B."
  (unless (and a b (equal (file-truename a) (file-truename b)))
    (string-prefix-p (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name b) t t)
                     (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name a) t t))))

(defmacro org-roam-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (unless (derived-mode-p 'org-mode)
         (delay-mode-hooks
           (let ((org-inhibit-startup t)
                 (org-agenda-files nil))
             (org-mode)
             (hack-local-variables))))
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

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

;;; Formatting
(defun org-roam-format-template (template replacer)
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
                   (if v
                       (format (apply #'propertize "%s" (text-properties-at 0 var)) v)
                     (signal 'org-roam-format-resolve md)))
               (set-match-data replacer-match-data))))
         (if (functionp template)
             (funcall template)
           template)
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

;;; Fontification
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
    (let ((org-ref-buffer-hacked t)
          (org-fold-core-style 'overlays))
      (org-mode)
      (font-lock-ensure)
      (buffer-string))))

;;;; Shielding regions
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

(defun org-roam-end-of-meta-data (&optional full)
  "Like `org-end-of-meta-data', but supports file-level metadata.

When FULL is non-nil but not t, skip planning information,
properties, clocking lines and logbook drawers.

When optional argument FULL is t, skip everything above, and also
skip keywords."
  (org-back-to-heading-or-point-min t)
  (when (org-at-heading-p) (forward-line))
  ;; Skip planning information.
  (when (looking-at-p org-planning-line-re) (forward-line))
  ;; Skip property drawer.
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line))
  ;; When FULL is not nil, skip more.
  (when (and full (not (org-at-heading-p)))
    (catch 'exit
      (let ((end (save-excursion (outline-next-heading) (point)))
            (re (concat "[ \t]*$" "\\|" org-clock-line-re)))
        (while (not (eobp))
          (cond ;; Skip clock lines.
           ((looking-at-p re) (forward-line))
           ;; Skip logbook drawer.
           ((looking-at-p org-logbook-drawer-re)
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                (forward-line)
              (throw 'exit t)))
           ((looking-at-p org-drawer-regexp)
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                (forward-line)
              (throw 'exit t)))
           ;; When FULL is t, skip keywords too.
           ((and (eq full t)
                 (looking-at-p org-keyword-regexp))
            (forward-line))
           (t (throw 'exit t))))))))

(defun org-roam-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (org-roam-end-of-meta-data 'drawers)
        (if (save-excursion (end-of-line) (eobp))
            (progn
              (end-of-line)
              (insert "\n"))
          (forward-line)
          (beginning-of-line))
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
  (org-roam-property-add prop val))

(defun org-roam-remove-property (prop &optional val)
  "Remove VAL value from PROP property for the node at point.
Both VAL and PROP are strings.

If VAL is not specified, user is prompted to select a value."
  (org-roam-property-remove prop val))

(defun org-roam-property-add (prop val)
  "Add VAL value to PROP property for the node at point.
Both, VAL and PROP are strings."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq val lst) lst (cons val lst)))
         (lst (seq-uniq lst)))
    (org-set-property prop (combine-and-quote-strings lst))
    val))

(defun org-roam-property-remove (prop &optional val)
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

;;; Refs
(defun org-roam-org-ref-path-to-keys (path)
  "Return a list of keys given an org-ref cite: PATH.
Accounts for both v2 and v3."
  (cond ((fboundp 'org-ref-parse-cite-path)
         (mapcar (lambda (cite) (plist-get cite :key))
                 (plist-get (org-ref-parse-cite-path path) :references)))
        ((fboundp 'org-ref-split-and-strip-string)
         (org-ref-split-and-strip-string path))))

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
  (let* ((toplib (or load-file-name buffer-file-name))
         gitdir topdir version)
    (unless (and toplib (equal (file-name-nondirectory toplib) "org-roam-utils.el"))
      (setq toplib (locate-library "org-roam-utils.el")))
    (setq toplib (and toplib (org-roam--straight-chase-links toplib)))
    (when toplib
      (setq topdir (file-name-directory toplib)
            gitdir (expand-file-name ".git" topdir)))
    (when (file-exists-p gitdir)
      (setq version
            (let ((default-directory topdir))
              (shell-command-to-string "git describe --tags --dirty --always"))))
    (unless version
      (setq version (with-temp-buffer
                      (insert-file-contents-literally (locate-library "org-roam.el"))
                      (goto-char (point-min))
                      (save-match-data
                        (if (re-search-forward "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                            (substring-no-properties (match-string 1))
                          "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

(defun org-roam--straight-chase-links (filename)
  "Chase links in FILENAME until a name that is not a link.

This is the same as `file-chase-links', except that it also
handles fake symlinks that are created by the package manager
straight.el on Windows.

See <https://github.com/raxod502/straight.el/issues/520>."
  (when (and (bound-and-true-p straight-symlink-emulation-mode)
             (fboundp 'straight-chase-emulated-symlink))
    (when-let ((target (straight-chase-emulated-symlink filename)))
      (unless (eq target 'broken)
        (setq filename target))))
  (file-chase-links filename))

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
    (insert (format "- Org-roam: %s" (org-roam-version)))
    (insert (format "- sqlite-connector: %s" org-roam-database-connector))))


(provide 'org-roam-utils)
;;; org-roam-utils.el ends here
