;;; org-roam-protocol.el --- Protocol handler for roam:// links

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1") (org "9.0"))

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
;; Intercept calls from emacsclient for `roam://' links.
;;
;; This is done by advising `server-visit-files' to scan the list of filenames
;; for `org-roam-protocol-the-protocol'.
;;
;; `roam://' links are expected to be absolute file locations, for example,
;; `roam:///home/me/file.org'. The `roam://' prefix is stripped, and emacsclient
;; opens the location as per usual.
;;
;; Any application that supports calling external programs with an URL as
;; argument may be used with this functionality.
;;
;; Usage:
;; ------
;;
;;    1.) Add this to your init file:
;;        (add-to-list 'load-path "/path/to/org-roam-protocol.el"')
;;        (require 'org-roam-protocol)
;;
;;    2.) Ensure emacs-server is up and running.
;;    3.) Try this from the command line:
;;        $ emacsclient roam:///tmp/test.org
;;
;;    If it works, you can now setup other applications for using this feature.

(require 'org)

;;; Variables:

(defconst org-roam-protocol-the-protocol "roam"
  "This is the protocol to detect if org-roam-protocol.el is loaded.
You will have to define just one protocl handler OS-wide (MS-Windows)
or per application (Linux). That protocol handler should call emacsclient.")

;;; Code:
(defun org-roam-protocol-check-filename-for-protocol (fname)
  "Check if `org-roam-protocol-the-protocol' is used in FNAME.

If the protocol is found, the protocol is stripped from fname,
and the value is passed to the server as filename.

If the function returns nil, the filename is removed from the
list of filenames passed from emacsclient to the server. If the
function returns a non-nil value, that value is passed to the
server as filename."
  (let ((the-protocol (concat (regexp-quote org-roam-protocol-the-protocol)
				                      ":")))
    (when (string-match the-protocol fname)
      (cadr (split-string fname the-protocol)))))

(defadvice server-visit-files (before org-roam-protocol-detect-protocol-server activate)
  "Advice `server-visit-files' to strip the `roam:/' protocol.
Default to `server-find-files' handling for file locations."
  (let ((flist (ad-get-arg 0)))
    (dolist (var flist)
      ;; `\' to '/' on windows.
      (let ((fname (expand-file-name (car var)))
            org-roam-location)
        (setq org-roam-location (org-roam-protocol-check-filename-for-protocol
                                 fname))
        (when (stringp org-roam-location) ; location for Org-roam file
          (setcar var org-roam-location))))))

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
