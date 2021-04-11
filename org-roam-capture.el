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
  "The node passed during an Org-roam capture. This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam-capture--info nil
  "A property-list of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defconst org-roam-capture--template-keywords '(:file-path :head :olp)
  "Keywords used in `org-roam-capture-templates' specific to Org-roam.")

(defcustom org-roam-capture-templates
  (list (list :key "d"
              :desc "default"
              :body "%?"
              :file-path "%<%Y%m%d%H%M%S>-${slug}.org"
              :head "#+title: ${title}\n"
              :unnarrowed t))
  "Capture templates for Org-roam.

TODO: Document this"
  :group 'org-roam
  :type '(repeat plist) ;; TODO: add :type properly
  )

(defcustom org-roam-capture-ref-templates
  (list (list :key "r"
              :desc "ref"
              :body "%?"
              :file-path "${slug}.org"
              :head "#+title: ${title}"
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

(cl-defun org-roam-capture--finalize-find-file (&key id)
  "Visit the buffer after Org-capture is done.
This function is to be called in the Org-capture finalization process.
ID is unused."
  (switch-to-buffer (org-capture-get :buffer)))

(cl-defun org-roam-capture--finalize-insert-link (&key id)
  "Insert a link to ID into the buffer where Org-capture was called.
ID is the Org id of the newly captured content.
This function is to be called in the Org-capture finalization process."
  (when-let* ((mkr (org-roam-capture--get :insert-at))
              (buf (marker-buffer mkr)))
    (with-current-buffer buf
      (when-let ((region (org-roam-capture--get :region)))
        (org-roam-unshield-region (car region) (cdr region))
        (delete-region (car region) (cdr region))
        (set-marker (car region) nil)
        (set-marker (cdr region) nil))
      (org-with-point-at mkr
        (insert (org-link-make-string (concat "id:" id)
                                      (org-roam-capture--get :link-description)))))))

(defun org-roam-capture--finalize-create-id ()
  "Get ID for newly captured information."
  (let ((buf (org-capture-get :buffer))
        (pos (org-capture-get :exact-position)))
    (with-current-buffer buf
      (org-with-point-at pos
        (org-id-get-create)))))

(defun org-roam-capture--add-ref (ref)
  "Add REF to the newly captured item."
  (let ((buf (org-capture-get :buffer))
        (pos (org-capture-get :exact-position))
        ref-lst)
    (with-current-buffer buf
      (org-with-point-at pos
        (setq ref-lst (org-entry-get (point) "ROAM_REFS"))
        (setq ref-lst (if ref-lst
                          (cl-pushnew (split-string-and-unquote ref-lst) ref)
                        (list ref)))
        (org-set-property "ROAM_REFS" (combine-and-quote-strings ref-lst))))))

(defun org-roam-capture--finalize ()
  "Finalize the `org-roam-capture' process."
  (let ((region (org-roam-capture--get :region))
        id)
    (when region
      (org-roam-unshield-region (car region) (cdr region)))
    (unless org-note-abort
      (setq id (org-roam-capture--finalize-create-id))
      (when-let ((ref (org-roam-capture--get :ref)))
        (org-roam-capture--add-ref ref))
      (when-let ((finalize (org-roam-capture--get :finalize)))
        (funcall (intern (concat "org-roam-capture--finalize-"
                                 (symbol-name (org-roam-capture--get :finalize))))
                 :id id)))
    (org-roam-capture--save-file-maybe)
    (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize)))

(defun org-roam-capture--install-finalize ()
  "Install `org-roam-capture--finalize' if the capture is an Org-roam capture."
  (when (org-roam-capture-p)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize)))

(add-hook 'org-capture-prepare-finalize-hook #'org-roam-capture--install-finalize)

(defun org-roam-capture--fill-template (str)
  "Expand the template STR, returning the string.
This is an extension of org-capture's template expansion.

First, it expands ${var} occurrences in STR, using the node in
`org-roam-capture--info'. Next, it expands the remaining template
string using `org-capture-fill-template'."
  (org-capture-fill-template
   (s-format str
             (lambda (key)
               (let ((fn (intern (concat "org-roam-node-" key))))
                 (if (fboundp fn)
                     (funcall fn org-roam-capture--node)
                   (completing-read (format "%s: " key) nil)))))))

(defun org-roam-capture--save-file-maybe ()
  "Save the file conditionally.
The file is saved if the original value of :no-save is not t and
`org-note-abort' is not t. It is added to
`org-capture-after-finalize-hook'."
  (cond
   ((and (org-roam-capture--get :new-file)
         org-note-abort)
    (with-current-buffer (org-capture-get :buffer)
      (set-buffer-modified-p nil)
      (kill-buffer)))
   ((and (not (org-roam-capture--get :orig-no-save))
         (not org-note-abort))
    (with-current-buffer (org-capture-get :buffer)
      (save-buffer)))))

(defun org-roam-capture--new-file ()
  "Return the path to file during an Org-roam capture.

This function reads the file-name attribute of the currently
active Org-roam template.

Else, to insert the header content in the file, `org-capture'
prepends the `:head' property of the Org-roam capture template.

To prevent the creation of a new file if the capture process is
aborted, we do the following:

1. Save the original value of the capture template's :no-save.
2. Set the capture template's :no-save to t.
3. Add a function on `org-capture-before-finalize-hook' that saves
the file if the original value of :no-save is not t and
`org-note-abort' is not t."
  (let* ((name-templ (or (org-roam-capture--get :file-path)
                         (user-error "Template needs to specify `:file-path'")))
         (rel-filename (s-trim (org-roam-capture--fill-template name-templ)))
         (file-path (expand-file-name rel-filename org-roam-directory))
         (roam-head (or (org-roam-capture--get :head) ""))
         (org-template (org-capture-get :template))
         (roam-template (concat roam-head org-template)))
    (if (or (file-exists-p file-path)
            (find-buffer-visiting file-path))
        (make-directory (file-name-directory file-path) t)
      (org-roam-capture--put :orig-no-save (org-capture-get :no-save)
                             :new-file t)
      (org-capture-put :template roam-template
                       :type 'plain)
      (org-capture-put :no-save t))
    file-path))

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
     (point-marker))))

(defun org-roam-capture--get-ref-path (type path)
  "Get the file path to the ref with TYPE and PATH."
  (caar (org-roam-db-query
         [:select [file]
          :from refs
          :where (= type $s1)
          :and (= ref $s2)
          :limit 1]
         type path)))

(defun org-roam-capture--get-point ()
  "Return exact point to file for org-capture-template.
This function is used solely in Org-roam's capture templates: see
`org-roam-capture-templates'."
  (let* ((file-path
          (cond ((plist-get org-roam-capture--info :ref)
                 (or (caar (org-roam-db-query [:select [file]
                                               :from refs
                                               :where (= ref $s1)
                                               :limit 1]
                                              (plist-get org-roam-capture--info :ref)))
                     (org-roam-capture--new-file)))
                ((org-roam-node-file org-roam-capture--node)
                 (org-roam-node-file org-roam-capture--node))
                (t
                 (org-roam-capture--new-file)))))
    (org-capture-put :template
                     (org-roam-capture--fill-template (org-capture-get :template)))
    (org-roam-capture--put :file-path file-path
                           :finalize (or (org-capture-get :finalize)
                                         (org-roam-capture--get :finalize)))
    (set-buffer (org-capture-target-buffer file-path))
    (widen)
    (if-let* ((olp (org-roam-capture--get :olp)))
        (condition-case err
            (when-let ((marker (org-roam-capture-find-or-create-olp olp)))
              (goto-char marker)
              (set-marker marker nil))
          (error
           (when (org-roam-capture--get :new-file)
             (kill-buffer))
           (signal (car err) (cdr err))))
      (goto-char (point-max)))))

(defun org-roam-capture--convert-template (template &optional props)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax.
PROPS is a plist containing additional Org-roam specific
properties to be added to the template."
  (let* ((key (or (plist-get template :key)
                  (user-error "Template has no :key")))
         (desc (or (plist-get template :desc)
                   (user-error "Template has no :desc")))
         (body (or (plist-get template :body)
                   (user-error "Template has no :body")))
         (rest (org-plist-delete template :key))
         (rest (org-plist-delete rest :desc))
         (rest (org-plist-delete rest :body))
         (rest (append rest props))
         org-roam-plist
         options)
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
    (append `(,key ,desc plain #'org-roam-capture--get-point ,body)
            options
            (list :org-roam org-roam-plist))))

;;;###autoload
(cl-defun org-roam-capture- (&key goto keys node info props templates)
  "Main entry point.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is an alist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates."
  (let* ((org-capture-templates
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
