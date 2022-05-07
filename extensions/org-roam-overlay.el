;;; org-roam-overlay.el --- Link overlay for [id:] links to Org-roam nodes -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
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
;; This extension allows to render [[id:]] links that don't have an associated
;; descriptor with an overlay that displays the node's current title.
;;
;;; Code:
(require 'org-roam)

(defface org-roam-overlay
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "black"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "white")))
  "Face for the Org-roam overlay."
  :group 'org-roam-faces)

(defun org-roam-overlay--make (l r &rest props)
  "Make an overlay from L to R with PROPS."
  (let ((o (make-overlay l (or r l))))
    (overlay-put o 'category 'org-roam)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun org-roam-overlay-make-link-overlay (link)
  "Create overlay for LINK."
  (save-excursion
    (save-match-data
      (let* ((type (org-element-property :type link))
             (id (org-element-property :path link))
             (pos (org-element-property :end link))
             (desc-p (org-element-property :contents-begin link))
             node)
        (when (and (string-equal type "id")
                   (setq node (org-roam-node-from-id id))
                   (not desc-p))
          (org-roam-overlay--make
           pos pos
           'after-string (format "%s "
                                 (propertize (org-roam-node-title node)
                                             'face 'org-roam-overlay))))))))

(defun org-roam-overlay-enable ()
  "Enable Org-roam overlays."
  (org-roam-db-map-links
   (list #'org-roam-overlay-make-link-overlay)))

(defun org-roam-overlay-disable ()
  "Disable Org-roam overlays."
  (remove-overlays nil nil 'category 'org-roam))

(defun org-roam-overlay-redisplay ()
  "Redisplay Org-roam overlays."
  (org-roam-overlay-disable)
  (org-roam-overlay-enable))

(define-minor-mode org-roam-overlay-mode
  "Overlays for Org-roam ID links.
Org-roam overlay mode is a minor mode.  When enabled,
overlay displaying the node's title is displayed."
  :lighter " org-roam-overlay"
  (if org-roam-overlay-mode
      (progn
        (org-roam-overlay-enable)
        (add-hook 'after-save-hook #'org-roam-overlay-redisplay nil t))
    (org-roam-overlay-disable)
    (remove-hook 'after-save-hook #'org-roam-overlay-redisplay t)))

(provide 'org-roam-overlay)
;;; org-roam-overlay.el ends here
