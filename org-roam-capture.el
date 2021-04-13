;;; org-roam-capture.el --- Capture functionality -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (magit-section "2.90.1"))

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
;; This library provides capture functionality for org-roam
;;; Code:
;;;; Library Requires
(require 'org-capture)
(eval-when-compile
  (require 'org-roam-macs))
(require 'org-roam-db)
(require 'dash)
(require 's)
(require 'cl-lib)

;; Declarations
(defvar org-roam-directory)

(defvar org-roam-capture--node nil
  "The node passed during an Org-roam capture.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam-capture--info nil
  "A property-list of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defconst org-roam-capture--template-keywords '(:if-new :id :link-description :call-location)
  "Keywords used in `org-roam-capture-templates' specific to Org-roam.")

(defcustom org-roam-capture-templates
  '(("d" "default" plain "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t))
  "Capture templates for Org-roam.

TODO: Document this"
  :group 'org-roam
  :type '(repeat plist) ;; TODO: add :type properly
  )

(defcustom org-roam-capture-ref-templates
  '(("r" "ref" plain "%?"
     :if-new (file+head "${slug}.org"
                        "#+title: ${title}")
     :unnarrowed t))
  "The Org-roam templates used during a capture from the roam-ref protocol.
Details on how to specify for the template is given in `org-roam-capture-templates'."
  :group 'org-roam
  :type '(repeat plist) ;; TODO: add :type properly
  )

(defun org-roam-capture-p ()
  "Return t if the current capture process is an Org-roam capture.
This function is to only be called when org-capture-plist is
valid for the capture (i.e. initialization, and finalization of
the capture)."
  (plist-get org-capture-plist :org-roam))

(defun org-roam-capture--get (keyword)
  "Get the value for KEYWORD from the `org-roam-capture-template'."
  (plist-get (plist-get org-capture-plist :org-roam) keyword))

(defun org-roam-capture--put (&rest stuff)
  "Put properties from STUFF into the `org-roam-capture-template'."
  (let ((p (plist-get org-capture-plist :org-roam)))
    (while stuff
      (setq p (plist-put p (pop stuff) (pop stuff))))
    (setq org-capture-plist
          (plist-put org-capture-plist :org-roam p))))

;; FIXME: Pending upstream patch
;; https://orgmode.org/list/87h7tv9pkm.fsf@hidden/T/#u
;;
;; Org-capture's behaviour right now is that `org-capture-plist' is valid only
;; during the initialization of the Org-capture buffer. The value of
;; `org-capture-plist' is saved into buffer-local `org-capture-current-plist'.
;; However, the value for that particular capture is no longer accessible for
;; hooks in `org-capture-after-finalize-hook', since the capture buffer has been
;; cleaned up.
;;
;; This advice restores the global `org-capture-plist' during finalization, so
;; the plist is valid during both initialization and finalization of the
;; capture.
(defun org-roam-capture--update-plist (&optional _)
  "Update global plist from local var."
  (setq org-capture-plist org-capture-current-plist))

(advice-add 'org-capture-finalize :before #'org-roam-capture--update-plist)

(defun org-roam-capture--finalize-find-file ()
  "Visit the buffer after Org-capture is done.
This function is to be called in the Org-capture finalization process.
ID is unused."
  (switch-to-buffer (org-capture-get :buffer)))

(defun org-roam-capture--finalize-insert-link ()
  "Insert a link to ID into the buffer where Org-capture was called.
ID is the Org id of the newly captured content.
This function is to be called in the Org-capture finalization process."
  (when-let* ((mkr (org-roam-capture--get :call-location))
              (buf (marker-buffer mkr)))
    (with-current-buffer buf
      (when-let ((region (org-roam-capture--get :region)))
        (org-roam-unshield-region (car region) (cdr region))
        (delete-region (car region) (cdr region))
        (set-marker (car region) nil)
        (set-marker (cdr region) nil))
      (org-with-point-at mkr
        (insert (org-link-make-string (concat "id:" (org-roam-capture--get :id))
                                      (org-roam-capture--get :link-description)))))))

(defun org-roam-capture--add-ref ()
  "Add REF to the newly captured item."
  (when-let ((ref (org-roam-capture--get :ref)))
    (let ((ref-lst (org-entry-get (point) "ROAM_REFS")))n
         (setq ref-lst (if ref-lst
                           (cl-pushnew (split-string-and-unquote ref-lst) ref)
                         (list ref)))
         (org-set-property "ROAM_REFS" (combine-and-quote-strings ref-lst)))
    (org-roam-capture--add-ref ref)))

(defun org-roam-capture--finalize ()
  "Finalize the `org-roam-capture' process."
  (when-let ((region (org-roam-capture--get :region)))
    (org-roam-unshield-region (car region) (cdr region)))
  (unless org-note-abort
    (when-let ((finalize (org-roam-capture--get :finalize)))
      (funcall (intern (concat "org-roam-capture--finalize-"
                               (symbol-name (org-roam-capture--get :finalize)))))))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize))

(defun org-roam-capture--install-finalize ()
  "Install `org-roam-capture--finalize' if the capture is an Org-roam capture."
  (when (org-roam-capture-p)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize)))

(add-hook 'org-capture-prepare-finalize-hook #'org-roam-capture--install-finalize)

(defun org-roam-capture--fill-template (str &optional org-capture-p)
  "Expand the template STR, returning the expanded template.
It expands ${var} occurrences in STR. When ORG-CAPTURE-P, also
run Org-capture's template expansion."
  (funcall (if org-capture-p #'org-capture-fill-template #'identity)
           (s-format str
                     (lambda (key)
                       (let ((fn (intern (concat "org-roam-node-" key))))
                         (if (fboundp fn)
                             (funcall fn org-roam-capture--node)
                           (completing-read (format "%s: " key) nil)))))))

(defun org-roam-capture--goto-location ()
  "Initialize the buffer, and goto the location of the new capture.
Return the ID of the location."
  (let (id)
    (pcase (or (org-roam-capture--get :if-new)
               (user-error "Template needs to specify `:if-new'"))
      (`(file ,path)
       (setq path (expand-file-name
                   (s-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (org-roam-capture--add-ref)
       (setq id (org-id-get-create)))
      (`(file+olp ,path ,olp)
       (setq path (expand-file-name
                   (s-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (set-buffer (org-capture-target-buffer path))
       (org-with-point-at 1
         (org-roam-capture--add-ref)
         (setq id (org-id-get-create)))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m))
       (widen))
      (`(file+head ,path ,head)
       (setq path (expand-file-name
                   (s-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (let ((exists-p (file-exists-p path)))
         (set-buffer (org-capture-target-buffer path))
         (unless exists-p
           (insert (org-roam-capture--fill-template head t))))
       (widen)
       (org-with-point-at 1
         (org-roam-capture--add-ref)
         (setq id (org-id-get-create))))
      (`(file+head+olp ,path ,head ,olp)
       (setq path (expand-file-name
                   (s-trim (org-roam-capture--fill-template path t))
                   org-roam-directory))
       (widen)
       (let ((exists-p (file-exists-p path)))
         (set-buffer (org-capture-target-buffer path))
         (unless exists-p
           (insert (org-roam-capture--fill-template head t))))
       (org-with-point-at 1
         (org-roam-capture--add-ref)
         (setq id (org-id-get-create)))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m)))
      ;; TODO: support node
      )
    id))

(defun org-roam-capture-find-or-create-olp (olp)
  "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
  (let* ((level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading olp)
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (org-insert-heading nil nil t)
           (unless (= lmax 1) (org-do-demote))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (copy-marker end))))

(defun org-roam-capture--get-ref-path (ref)
  "Return the file and point of reference REF."
  (save-match-data
    (when (string-match org-link-plain-re ref)
      (let ((type (match-string 1 ref))
            (path (match-string 2 ref)))
        (car (org-roam-db-query
              [:select [nodes:file pos]
               :from refs
               :left-join nodes
               :on (= refs:node-id nodes:id)
               :where (= refs:type $s1)
               :and (= refs:ref $s2)
               :limit 1]
              type path))))))

(defun org-roam-capture--get-point ()
  "Return exact point to file for org-capture-template.
This function is used solely in Org-roam's capture templates: see
`org-roam-capture-templates'."
  (let ((id (cond ((plist-get org-roam-capture--info :ref)
                   (if-let ((file-pos (org-roam-capture--get-ref-path
                                       (plist-get org-roam-capture--info :ref))))
                       (progn
                         (set-buffer (org-capture-target-buffer (car file-pos)))
                         (goto-char (cdr file-pos))
                         (widen))
                     (org-roam-capture--goto-location)))
                  ((and (org-roam-node-file org-roam-capture--node)
                        (org-roam-node-point org-roam-capture--node))
                   (set-buffer (org-capture-target-buffer (org-roam-node-file org-roam-capture--node)))
                   (goto-char (org-roam-node-point org-roam-capture--node))
                   (widen)
                   (org-end-of-subtree t t)
                   (org-roam-node-id org-roam-capture--node))
                  (t
                   (org-roam-capture--goto-location)))))
    (org-capture-put :template
                     (org-roam-capture--fill-template (org-capture-get :template)))
    (org-roam-capture--put :id id)
    (org-roam-capture--put :finalize (or (org-capture-get :finalize)
                                         (org-roam-capture--get :finalize)))))

(defun org-roam-capture--convert-template (template &optional props)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax.
PROPS is a plist containing additional Org-roam specific
properties to be added to the template."
  (pcase template
    (`(,key ,desc ,type ,body . ,rest)
     (setq rest (append rest props))
     (let (org-roam-plist options)
       (while rest
         (let* ((key (pop rest))
                (val (pop rest))
                (custom (member key org-roam-capture--template-keywords)))
           (when (and custom
                      (not val))
             (user-error "Invalid capture template format: %s\nkey %s cannot be nil" template key))
           (if custom
               (setq org-roam-plist (plist-put org-roam-plist key val))
             (setq options (plist-put options key val)))))
       (append `(,key ,desc ,type #'org-roam-capture--get-point ,body)
               options
               (list :org-roam org-roam-plist))))
    (_
     (signal 'invalid-template template))))

;;;###autoload
(cl-defun org-roam-capture- (&key goto keys node info props templates)
  "Main entry point.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is an alist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates."
  (let* ((m (point-marker))
         (props (plist-put props :call-location m))
         (org-capture-templates
          (mapcar (lambda (t)
                    (org-roam-capture--convert-template t props))
                  (or templates org-roam-capture-templates)))
         (org-roam-capture--node node)
         (org-roam-capture--info info))
    (when (and (not keys)
               (= (length org-capture-templates) 1))
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)))

;;;###autoload
(defun org-roam-capture (&optional goto keys)
  "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'."
  (interactive "P")
  (let ((node (org-roam-node-read)))
    (org-roam-capture- :goto goto
                       :keys keys
                       :node node
                       :props '(:immediate-finish nil))))

(provide 'org-roam-capture)

;;; org-roam-capture.el ends here
