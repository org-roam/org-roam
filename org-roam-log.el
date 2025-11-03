;;; org-roam-log.el --- Integrations with Org-log -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2022-2025 Jethro Kuan <jethrokuan95@gmail.com>

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
;; This module provides integrations with Org-log.
;;
;;; Code:
(require 'org-roam)

(defcustom org-roam-log-setup-hook nil
  "Hook run when a log for an Org-roam file is setup."
  :group 'org-roam
  :type 'hook)

(defun org-roam-log-p ()
  "Return t if the log buffer is for an Org-roam file, nil otherwise."
  (and org-log-note-marker
       (org-roam-file-p (buffer-file-name (marker-buffer org-log-note-marker)))))

(defun org-roam-log--setup ()
  "Run hooks in `org-roam-log-setup-hook'."
  (run-hooks 'org-roam-log-setup-hook))

(add-hook 'org-roam-log-setup-hook #'org-roam--register-completion-functions-h)
(add-hook 'org-log-buffer-setup-hook #'org-roam-log--setup)

(provide 'org-roam-log)
;;; org-roam-log.el ends here
