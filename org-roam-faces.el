;;; org-roam-faces.el --- Face definitions -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1"))

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

;; This file contains the face definitions for Org-roam.

;;; Code:

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

;;; Definitions
(defface org-roam-link
  '((t :inherit org-link))
  "Face for Org-roam links."
  :group 'org-roam-faces)

(defface org-roam-tag
  '((t :weight bold))
  "Face for Org-roam tags in minibuffer commands."
  :group 'org-roam-faces)

(defface org-roam-link-current
  '((t :inherit org-link))
  "Face for Org-roam links pointing to the current buffer."
  :group 'org-roam-faces)

(defface org-roam-link-invalid
  '((t :inherit (error org-link)))
  "Face for Org-roam links that are not valid.
This face is used for links without a destination."
  :group 'org-roam-faces)

(defface org-roam-link-shielded
  '((t :inherit (warning org-link)))
  "Face for Org-roam links that are shielded.
This face is used on the region target by `org-roam-insertion'
during an `org-roam-capture'."
  :group 'org-roam-faces)

(defface org-roam-dailies-calendar-note
  '((t :inherit (org-roam-link) :underline nil))
  "Face for dates with a daily-note in the calendar"
  :group 'org-roam-faces)

;;; _

(provide 'org-roam-faces)

;;; org-roam-faces.el ends here
