;;; org-roam-transient.el --- transient menu for org-roam  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (org "9.3") (transient "0.2.0"))

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
;; Add a transient menu (https://magit.vc/manual/transient/) for
;; org-roam. To use the menu, bind `org-roam-menu' to a key, e.g,
;; (using the `bind-key' package from
;; https://github.com/priyadarshan/bind-key):
;;
;; (bind-key "C-c n" 'org-roam-menu)
;;
;;; Code:
(require 'transient)

;;;###autoload
(transient-define-prefix org-roam-menu ()
  "Transient menu for org-roam."
  [("l" "show/hide org-roam buffer" org-roam)
   ("f" "open an org-roam file" org-roam-find-file)
   ("g" "display the org-roam graph" org-roam-graph)
   ("i" "insert org-roam link" org-roam-insert)
   ("I" "insert org-roam link using template" org-roam-insert-immediate)])

(provide 'org-roam-transient)

;;; org-roam-transient.el ends here
