;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "2.90.1"))

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
;; 1. "roam-node": This protocol simply opens the node given by the node ID
;; 2. "roam-ref": This protocol creates or opens a note with the given REF
;;
;;; Code:
(require 'org-protocol)
(require 'org-roam)
(eval-when-compile
  (require 'org-roam-macs))
(require 'ol) ;; for org-link-decode

(defcustom org-roam-protocol-store-links nil
  "Whether to store links when capturing websites with `org-roam-protocol'."
  :type 'boolean
  :group 'org-roam)

;;;; Functions
(defun org-roam-protocol-open-ref (info)
  "Process an org-protocol://roam-ref?ref= style url with INFO.

It opens or creates a note with the given ref.

  javascript:location.href = \\='org-protocol://roam-ref?template=r&ref=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' + \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())"
  (unless (plist-get info :ref)
    (user-error "No ref key provided"))
  (org-roam-plist-map! (lambda (k v)
                         (org-link-decode
                          (if (equal k :ref)
                              (org-protocol-sanitize-uri v)
                            v))) info)
  (when org-roam-protocol-store-links
    (push (list (plist-get info :ref)
                (plist-get info :title)) org-stored-links))
  (org-link-store-props :type (and (string-match org-link-plain-re
                                                 (plist-get info :ref))
                                   (match-string 1 (plist-get info :ref)))
                        :link (plist-get info :ref)
                        :annotation (org-link-make-string (plist-get info :ref)
                                                          (or (plist-get info :title)
                                                              (plist-get info :ref)))
                        :initial (or (plist-get info :body) ""))
  (raise-frame)
  (org-roam-capture-
   :keys (plist-get info :template)
   :node (org-roam-node-create :title (plist-get info :title))
   :info (list :ref (plist-get info :ref)
               :body (plist-get info :body))
   :templates org-roam-capture-ref-templates)
  nil)

(defun org-roam-protocol-open-node (info)
  "This handler simply opens the file with emacsclient.

INFO is an alist containing additional information passed by the protocol URL.
It should contain the FILE key, pointing to the path of the file to open.

  Example protocol string:

org-protocol://roam-node?node=uuid"
  (when-let ((node (plist-get info :node)))
    (raise-frame)
    (org-roam-node-visit (org-roam-populate (org-roam-node-create :id node))))
  nil)

(push '("org-roam-ref"  :protocol "roam-ref"   :function org-roam-protocol-open-ref)
      org-protocol-protocol-alist)
(push '("org-roam-node"  :protocol "roam-node"   :function org-roam-protocol-open-node)
      org-protocol-protocol-alist)

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
