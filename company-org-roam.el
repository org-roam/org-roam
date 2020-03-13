;;; company-org-roam.el --- Company backend for Org-roam

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.0.0-rc1
;; Package-Requires: ((emacs "26.1") (company "0.9.0"))

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

;; `company-org-roam' is a `company' completion backend for Org-roam.
;; To use it, add `company-org-roam' to `company-backends':

;;     (require 'company-org-roam)
;;     (company-org-roam-init)

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'org-roam)

(defgroup company-org-roam nil
  "Company completion backend for Org-roam."
  :prefix "company-org-roam-"
  :group 'org-roam)

(defcustom company-org-roam-cache-expire 10
  "Number of seconds before the caches expire.

A value of nil means the caches never expire."
  :type '(integer :tag "Seconds")
  :group 'company-org-roam)

(defvar company-org-roam-last-cache-time (make-hash-table :test #'equal)
  "Time of last cache.")

(defvar company-org-roam-cache (make-hash-table :test #'equal)
  "In-memory cache for completions.")

(defun company-org-roam-time-seconds ()
  "Return the number of seconds since the unix epoch."
  (cl-destructuring-bind (high low _usec _psec) (current-time)
    (+ (lsh high 16) low)))

(defun company-org-roam--post-completion (candidate)
  "The post-completion action for `company-org-roam'.
It deletes the inserted CANDIDATE, and replaces it with a
relative file link."
  (let* ((cache (gethash (file-truename org-roam-directory) company-org-roam-cache))
         (path (gethash candidate cache))
         (current-file-path (-> (or (buffer-base-buffer)
                                    (current-buffer))
                                (buffer-file-name)
                                (file-truename)
                                (file-name-directory))))
    (delete-region (- (point) (length candidate)) (point))
    (insert (format "[[file:%s][%s]]"
                    (file-relative-name path current-file-path)
                    candidate))))

(defun company-org-roam--filter-candidates (prefix candidates)
  "Filter CANDIDATES that start with PREFIX.
The string match is case-insensitive."
  (-filter (lambda (candidate)
             (s-starts-with-p prefix candidate t)) candidates))

(defun company-org-roam--update-cache ()
  "Update the cache with new entries.
Entries with no title do not appear in the completions."
  (let ((dir (file-truename org-roam-directory))
        (ht (make-hash-table :test #'equal)))
    (dolist (row (org-roam-sql [:select [titles file] :from titles]))
      (let ((titles (car row))
            (file (cadr row)))
        (dolist (title titles)
          (puthash title file ht))))
    (puthash dir ht company-org-roam-cache)))

(defun company-org-roam--cache-get-titles ()
  "Return all the titles."
  (let* ((dir (file-truename org-roam-directory))
         (last-cache-time (gethash dir company-org-roam-last-cache-time))
         (curr-time (company-org-roam-time-seconds)))
    (when (or (null last-cache-time)
              (< (+ last-cache-time company-org-roam-cache-expire)
                 curr-time))
      (puthash dir curr-time company-org-roam-last-cache-time)
      (company-org-roam--update-cache))
    (hash-table-keys (gethash dir company-org-roam-cache))))

(defun company-org-roam--get-candidates (prefix)
  "Get the candidates for PREFIX."
  (->> (company-org-roam--cache-get-titles)
       (-flatten)
       (company-org-roam--filter-candidates prefix)))

;;;###autoload
(defun company-org-roam (command &optional arg &rest _)
  "Define a company backend for Org-roam.
COMMAND and ARG are as per the documentation of `company-backends'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-org-roam))
    (prefix
     (and
      (bound-and-true-p org-roam-mode)
      (or (company-grab-symbol) 'stop)))
    (candidates
     (company-org-roam--get-candidates arg))
    (post-completion (company-org-roam--post-completion arg))))

(defun company-org-roam--init-hook ()
  "Conditional enabling of the `company-org-roam' backend."
  (when (org-roam--org-roam-file-p (buffer-file-name (buffer-base-buffer)))
    (setq-local company-backends
                (cons'company-org-roam company-backends))))

;;;###autoload
(defun company-org-roam-init ()
  "Injects `company-org-roam' as a completion backend."
  (add-hook 'org-mode-hook #'company-org-roam--init-hook))

(provide 'company-org-roam)

;;; company-org-roam.el ends here
