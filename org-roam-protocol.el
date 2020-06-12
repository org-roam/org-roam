;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.0
;; Package-Requires: ((emacs "26.1") (org "9.3"))

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
;; We define 2 protocols:
;;
;; 1. "roam-file": This protocol simply opens the file given by the FILE key
;; 2. "roam-ref": This protocol creates or opens a note with the given REF
;;
;;; Code:
(require 'org-protocol)
(require 'org-roam)

;;;; Functions
(defun org-roam-protocol-open-ref (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

It opens or creates a note with the given ref.

  javascript:location.href = \\='org-protocol://roam-ref?template=r&ref=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())"
  (when-let* ((alist (org-roam--plist-to-alist info))
              (decoded-alist (mapcar (lambda (k.v)
                                       (let ((key (car k.v))
                                             (val (cdr k.v)))
                                         (cons key (org-link-decode val)))) alist)))
    (unless (assoc 'ref decoded-alist)
      (error "No ref key provided"))
    (when-let ((title (cdr (assoc 'title decoded-alist))))
      (push (cons 'slug (org-roam--title-to-slug title)) decoded-alist))
    (let* ((org-roam-capture-templates org-roam-capture-ref-templates)
           (org-roam-capture--context 'ref)
           (org-roam-capture--info decoded-alist)
           (template (cdr (assoc 'template decoded-alist))))
      (raise-frame)
      (org-roam--with-template-error 'org-roam-capture-ref-templates
        (org-roam-capture--capture nil template))
      (org-roam-message "Item captured.")))
  nil)

(defun org-roam-protocol-open-file (info)
  "This handler simply opens the file with emacsclient.

INFO is an alist containing additional information passed by the protocol URL.
It should contain the FILE key, pointing to the path of the file to open.

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
