;;; org-roam-helm-bibtex.el --- Org-roam helm-bibtex integration -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Jethro Kuan <jethrokuan95@gmail.com>
;; Created: 12 April 2020
;; Keywords: bibtex, refs

;; This file is not part of GNU Emacs.

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

;; This script provides basic interaction between `org-roam' and `helm-bibtex'
;; by enabling bibliographic notes stored in `org-roam-directory' to be
;; accessed from `helm-bibtex' or `ivy-bibtex'.  If a note does not exist, the
;; script creates one.

;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'org-roam)
(require 'bibtex)
(require 'bibtex-completion)

(defun org-roam-bibtex-completion-edit-notes (keys)
  "Open the notes associated with the selected entries using `find-file'."
  (dolist (key keys)
    (let ((refs (org-roam--get-ref-path-completions)))
      (if-let ((path (cdr (assoc key refs))))
          (find-file path)
        (let* ((title key)
               (org-roam-capture--info (list (cons 'title title)
                                             (cons 'ref (format "cite:%s" key))
                                             (cons 'slug (org-roam--title-to-slug key))))
               (org-roam-capture--context 'ref)
               (org-roam-capture-templates org-roam-capture-ref-templates))
          (org-roam--capture))))))

(advice-add 'bibtex-completion-edit-notes :override #'org-roam-bibtex-completion-edit-notes)

(provide 'org-roam-helm-bibtex)
;;; org-roam-helm-bibtex.el ends here
