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
;; You can find detailed instructions on how to setup the protocol in the
;; documentation for Org-roam.
;;
;;; Code:
(require 'org-protocol)
(require 'ol) ;; for org-link-decode
(require 'org-roam)

;;; Options
(defcustom org-roam-protocol-store-links nil
  "Whether to store links when capturing websites with `org-roam-protocol'."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-capture-ref-templates
  '(("r" "ref" plain "%?"
     :target (file+head "${slug}.org"
                        "#+title: ${title}")
     :unnarrowed t))
  "The Org-roam templates used during a capture from the roam-ref protocol.
See `org-roam-capture-templates' for the template documentation."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :target)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

;;; Handlers
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
  (let ((org-capture-link-is-already-stored t))
    (org-roam-capture-
     :keys (plist-get info :template)
     :node (org-roam-node-create :title (plist-get info :title))
     :info (list :ref (plist-get info :ref)
                 :body (plist-get info :body))
     :templates org-roam-capture-ref-templates))
  nil)

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

(push '("org-roam-ref"  :protocol "roam-ref"   :function org-roam-protocol-open-ref)
      org-protocol-protocol-alist)
(push '("org-roam-node"  :protocol "roam-node"   :function org-roam-protocol-open-node)
      org-protocol-protocol-alist)

;;; Capture implementation
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


(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
