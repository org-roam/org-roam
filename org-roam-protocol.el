;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>

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
;; We extend org-protocol, adding custom Org-roam handlers. The setup
;; instructions for `org-protocol' can be found in org-protocol.el.
;;
;;; Code:

(require 'org-protocol)
(require 'org-roam)

(declare-function org-roam-find-ref "org-roam" (&optional info))

(defun org-roam-protocol-open-ref (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function decodes a ref, and places it into
This function detects an file, and opens it.

  javascript:location.href = \\='org-protocol://roam-ref?ref=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())"
  (when-let* ((alist (org-roam--plist-to-alist info))
              (decoded-alist (mapcar (lambda (k.v)
                                       (let ((key (car k.v))
                                             (val (cdr k.v)))
                                         (cons key (org-link-decode val)))) alist)))
    (when (assoc 'ref decoded-alist)
      (raise-frame)
      (org-roam-find-ref decoded-alist)))
  nil)

(defun org-roam-protocol-open-file (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

  Example protocol string:

org-protocol://roam-file?file=/path/to/file.org"
  (when-let ((file (plist-get info :file)))
    (raise-frame)
    (find-file file))
  nil)

(push '("org-roam-ref"  :protocol "roam-ref"   :function org-roam-protocol-open-ref)
      org-protocol-protocol-alist)
(push '("org-roam-file"  :protocol "roam-file"   :function org-roam-protocol-open-file)
      org-protocol-protocol-alist)

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
