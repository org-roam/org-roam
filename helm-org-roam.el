;;; helm-org-roam.el --- Helm support for Org-roam

;; Copyright © 2020 Konrad Hinsen

;; Author: Konrad Hinsen
;; URL: https://github.com/khinsen/org-roam
;; Keywords: org-mode, roam, helm
;; Version: 1.0.0-rc1
;; Package-Requires: ((emacs "26.1") (helm-core "3.5.0"))

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

;; `helm-org-roam' adds `helm' support to Org-roam. It defines
;; the interactive commands `helm-org-roam-find-file' and
;; `helm-org-roam-insert' that behave like the standard
;; `org-roam-find-file' and `org-roam-insert' except for
;; using `helm' for completion.

;;; Code:

(require 'helm)
(require 'org-roam)
(require 's)

(defun helm-org-roam---candidate-transformer (candidates source)
  (let ((prefixed-pattern (propertize
                           " " 'display
                           (propertize "[?]" 'face 'helm-ff-prefix))))
    (cons (concat prefixed-pattern " " helm-pattern)
          candidates)))

(defun helm-org-roam--read-title (completions &optional input)
  (let ((title
         (helm :sources (helm-build-sync-source "Title"
                          :candidates (-map #'car completions)
                          :filtered-candidate-transformer
                          #'helm-org-roam---candidate-transformer
                          :fuzzy-match t)
               :buffer "*org-roam titles*"
               :prompt "Title: "
               :input input)))
    (unless title
      (keyboard-quit))
    (s-trim-left title)))

(defun helm-org-roam-find-file ()
  "Find and open an org-roam file using Helm."
  (interactive)
  (org-roam--find-file-with-completion-method #'helm-org-roam--read-title))

(defun org-roam-insert (prefix)
  "Find an org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method
   prefix
   #'(lambda (completions region-text)
       (completing-read "File: " completions nil nil region-text))))

(defun helm-org-roam-insert (prefix)
  "Find an org-roam file using Helm, and insert a relative org link to
it at point. If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method 
   prefix #'helm-org-roam--read-title))
