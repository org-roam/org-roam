;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.1.0
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
;; This extension extends `org-protocol', adding custom Org-roam handlers to it
;; to provide the next new protocols:
;;
;; 1. "roam-node": This protocol simply opens the node given by the node ID
;; 2. "roam-ref": This protocol creates or opens the node with the given REF
;;
;; To add new capture templates dedicated for the protocol, specify ":kind
;; protocol" for each of such template in `org-roam-capture-templates', e.g.
;;
;;   (setq org-roam-capture-templates
;;          '(("r" "ref" plain "%?" :kind protocol
;;             :if-new (file+head "${slug}.org"
;;                                "#+title: ${title}")
;;             :unnarrowed t)))
;;
;; You can find a detailed instruction on how to setup the protocol in the
;; manual for Org-roam.
;;
;;; Code:
(require 'org-protocol)
(require 'ol) ; to use `org-link-decode'
(require 'org-roam)

;;; Options
(defcustom org-roam-protocol-store-links nil
  "Whether to store links when capturing websites with `org-roam-protocol'."
  :type 'boolean
  :group 'org-roam)

;;; Protocols
(mapc (lambda (spec) (cl-pushnew spec org-protocol-protocol-alist :test #'equal))
      '(("org-roam-ref"   :protocol "roam-ref"    :function org-roam-protocol-open-ref)
        ("org-roam-node"  :protocol "roam-node"   :function org-roam-protocol-open-node)))

;;;; roam-ref
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
   :props '(:kind protocol))
  nil)

(add-hook 'org-roam-capture-preface-hook #'org-roam-protocol--try-capture-to-ref-h)
(defun org-roam-protocol--try-capture-to-ref-h ()
  "Try to capture to an existing node that match the ref."
  (when-let ((node (and (plist-get org-roam-capture--info :ref)
                        (org-roam-node-from-ref
                         (plist-get org-roam-capture--info :ref)))))
    (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
    (goto-char (org-roam-node-point node))
    (widen)
    (org-roam-node-id node)))

(add-hook 'org-roam-capture-new-node-hook #'org-roam-protocol--insert-captured-ref-h)
(defun org-roam-protocol--insert-captured-ref-h ()
  "Insert the ref if any."
  (when-let ((ref (plist-get org-roam-capture--info :ref)))
    (org-roam-ref-add ref)))

(when (org-roam-capture--load-templates-p 'org-roam-protocol)
  (push '("r" "ref" plain "%?" :kind protocol
          :if-new (file+head "${slug}.org"
                             "#+title: ${title}")
          :unnarrowed t)
        org-roam-capture-templates))

;;;; roam-node
(defun org-roam-protocol-open-node (info)
  "This handler simply opens the file with emacsclient.

INFO is a plist containing additional information passed by the protocol URL.
It should contain the FILE key, pointing to the path of the file to open.

  Example protocol string:

org-protocol://roam-node?node=uuid"
  (when-let ((node (plist-get info :node)))
    (raise-frame)
    (org-roam-node-visit (org-roam-populate (org-roam-node-create :id node)) nil 'force))
  nil)

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
