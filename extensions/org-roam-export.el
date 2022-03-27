;;; org-roam-export.el --- Org-roam org-export tweaks -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.1
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-roam "2.1"))

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
;; This package provides the necessary changes required to make org-export work out-of-the-box.
;;
;; To enable it, run:
;;
;;    (require 'org-roam-export)
;;
;; The key issue Org's export-to-html functionality has is that it does not respect the ID property, which
;; Org-roam relies heavily on. This patches the necessary function in ox-html to export ID links correctly,
;; pointing to the correct place.
;;
;;; Code:
(require 'ox-html)

(defun org-roam-export--org-html--reference (datum info &optional named-only)
  "Org-roam's patch for `org-html--reference' to support ID link export.
See `org-html--reference' for DATUM, INFO and NAMED-ONLY."
  (let* ((type (org-element-type datum))
         (user-label
          (org-element-property
           (pcase type
             ((or `headline `inlinetask) :CUSTOM_ID)
             ((or `radio-target `target) :value)
             (_ :name))
           datum))
         (user-label
          (or user-label
              (when-let ((path (org-element-property :ID datum)))
                ;; see `org-html-link' for why we use "ID-"
                ;; (search for "ID-" in ox-html.el)
                (concat "ID-" path)))))
    (cond
     ((and user-label
           (or (plist-get info :html-prefer-user-labels)
               (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))

(advice-add 'org-html--reference :override #'org-roam-export--org-html--reference)

(provide 'org-roam-export)
;;; org-roam-export.el ends here
