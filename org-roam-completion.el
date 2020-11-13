;;; org-roam-completion.el --- Completion features -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
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
;; This library provides completion for org-roam.
;;; Code:
;;;; Library Requires
(require 'cl-lib)
(require 's)

(defvar helm-pattern)
(declare-function helm "ext:helm")
(declare-function helm-make-source "ext:helm-source" (name class &rest args) t)

(defcustom org-roam-completion-system 'default
  "The completion system to be used by `org-roam'."
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :group 'org-roam)

(defcustom org-roam-completion-ignore-case t
  "Whether to ignore case in Org-roam `completion-at-point' completions."
  :group 'org-roam
  :type 'boolean)

(defun org-roam-completion--helm-candidate-transformer (candidates _source)
  "Transforms CANDIDATES for Helm-based completing read.
SOURCE is not used."
  (let ((prefix (propertize "[?] "
                            'face 'helm-ff-prefix)))
    (cons (propertize helm-pattern
                      'display (concat prefix helm-pattern))
          candidates)))

(cl-defun org-roam-completion--completing-read (prompt choices &key
                                                       require-match initial-input
                                                       action)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
Return user choice."
  (let (res)
    (setq res
          (cond
           ((eq org-roam-completion-system 'ido)
            (let ((candidates (mapcar #'car choices)))
              (ido-completing-read prompt candidates nil require-match initial-input)))
           ((eq org-roam-completion-system 'default)
            (completing-read prompt choices nil require-match initial-input))
           ((eq org-roam-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :preselect initial-input
                          :require-match require-match
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'org-roam--completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           ((eq org-roam-completion-system 'helm)
            (unless (and (fboundp 'helm)
                         (fboundp 'helm-make-source))
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
            (let ((source (helm-make-source prompt 'helm-source-sync
                            :candidates (mapcar #'car choices)
                            :filtered-candidate-transformer
                            (and (not require-match)
                                 #'org-roam-completion--helm-candidate-transformer)))
                  (buf (concat "*org-roam "
                               (s-downcase (s-chop-suffix ":" (s-trim prompt)))
                               "*")))
              (or (helm :sources source
                        :action (if action
                                    (prog1 action
                                      (setq action nil))
                                  #'identity)
                        :prompt prompt
                        :input initial-input
                        :buffer buf)
                  (keyboard-quit))))))
    (if action
        (funcall action res)
      res)))

(provide 'org-roam-completion)

;;; org-roam-completion.el ends here
