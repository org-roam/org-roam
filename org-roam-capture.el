;;; org-roam-capture.el --- Capture functionality -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.0"))

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
(require 'dash)
(require 's)
(require 'cl-lib)

;; Declarations
(defvar org-roam-encrypt-files)
(defvar org-roam-directory)
(defvar org-roam-mode)
(declare-function  org-roam--get-title-path-completions "org-roam")
(declare-function  org-roam--get-ref-path-completions   "org-roam")
(declare-function  org-roam--file-path-from-id          "org-roam")
(declare-function  org-roam--format-link                "org-roam")
(declare-function  org-roam--title-to-slug              "org-roam")
(declare-function  org-roam-mode                        "org-roam")
(declare-function  org-roam-completion--completing-read "org-roam-completion")

(defvar org-roam-capture--file-name-default "%<%Y%m%d%H%M%S>"
  "The default file name format for Org-roam templates.")

(defvar org-roam-capture--header-default "#+title: ${title}\n"
  "The default capture header for Org-roam templates.")

(defvar org-roam-capture--file-path nil
  "The file path for the Org-roam capture.
This variable is set during the Org-roam capture process.")

(defvar org-roam-capture--info nil
  "An alist of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam-capture--context nil
  "A symbol, that reflects the context for obtaining the exact point in a file.
This variable is populated dynamically, and is only active during
an Org-roam capture process.

The `title' context is used in `org-roam-insert' and
`org-roam-find-file', where the capture process is triggered upon
trying to create a new file without that `title'.

The `ref' context is used by `org-roam-protocol', where the
capture process is triggered upon trying to find or create a new
note with the given `ref'.")

(defvar org-roam-capture-additional-template-props nil
  "Additional props to be added to the Org-roam template.")

(defconst org-roam-capture--template-keywords '(:file-name :head)
  "Keywords used in `org-roam-capture-templates' specific to Org-roam.")

(defvar org-roam-capture-templates
  '(("d" "default" plain (function org-roam-capture--get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+title: ${title}\n"
     :unnarrowed t))
  "Capture templates for Org-roam.
The capture templates are an extension of
`org-capture-templates', and the documentation there also
applies.

`org-capture-templates' are extended in 3 ways:

1. Template expansion capabilities are extended with additional custom syntax.
   See `org-roam-capture--fill-template' for more details.

2. The `:file-name' key is added, which expands to the file-name
   of the note if it creates a new file.  This file-name is
   relative to `org-roam-directory', and is without the
   file-extension.

3. The `:head' key is added, which contains the template that is
   inserted on initial creation (added only once).  This is where
   insertion of any note metadata should go.")

(defvar org-roam-capture-ref-templates
  '(("r" "ref" plain (function org-roam-capture--get-point)
     ""
     :file-name "${slug}"
     :head "#+title: ${title}
#+roam_key: ${ref}\n"
     :unnarrowed t))
  "The Org-roam templates used during a capture from the roam-ref protocol.
Details on how to specify for the template is given in `org-roam-capture-templates'.")

(defun org-roam-capture--get (keyword)
  "Gets the value for KEYWORD from the `org-roam-capture-template'."
  (plist-get (plist-get org-capture-plist :org-roam) keyword))

(defun org-roam-capture--put (&rest stuff)
  "Puts properties from STUFF into the `org-roam-capture-template'."
  (let ((p (plist-get org-capture-plist :org-roam)))
    (while stuff
      (setq p (plist-put p
                         (pop stuff) (pop stuff))))
    (setq org-capture-plist
          (plist-put org-capture-plist :org-roam p))))

(defun org-roam-capture--in-process-p ()
  "Return non-nil if a `org-roam-capture' buffer exists."
  (cl-some (lambda (buffer)
             (and (eq (buffer-local-value 'major-mode buffer)
                      'org-mode)
                  (plist-get (buffer-local-value 'org-capture-current-plist buffer)
                             :org-roam)))
           (buffer-list)))

(defun org-roam-capture--fill-template (str &optional info)
  "Expands the template STR, returning the string.
This is an extension of org-capture's template expansion.

First, it expands ${var} occurrences in STR, using the INFO alist.
If there is a ${var} with no matching var in the alist, the value
of var is prompted for via `completing-read'.

Next, it expands the remaining template string using
`org-capture-fill-template'."
  (-> str
      (s-format (lambda (key)
                  (or (s--aget info key)
                      (completing-read (format "%s: " key ) nil))) nil)
      (org-capture-fill-template)))

(defun org-roam-capture--insert-link-h ()
  "Insert the link into the original buffer, after the capture process is done.
This is added as a hook to `org-capture-after-finalize-hook'."
  (when (and (not org-note-abort)
             (eq (org-roam-capture--get :capture-fn)
                 'org-roam-insert))
    (when-let ((region (org-roam-capture--get :region))) ;; Remove previously selected text.
      (delete-region (car region) (cdr region)))
    (insert (org-roam--format-link (org-roam-capture--get :file-path)
                                   (org-roam-capture--get :link-description))))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--insert-link-h))

(defun org-roam-capture--save-file-maybe-h ()
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
      (save-buffer))))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--save-file-maybe-h))

(defun org-roam-capture--new-file ()
  "Return the path to the new file during an Org-roam capture.

This function reads the file-name attribute of the currently
active Org-roam template.

If the file path already exists, it throw an error.

Else, to insert the header content in the file, `org-capture'
prepends the `:head' property of the Org-roam capture template.

To prevent the creation of a new file if the capture process is
aborted, we do the following:

1. Save the original value of the capture template's :no-save.

2. Set the capture template's :no-save to t.

3. Add a function on `org-capture-after-finalize-hook' that saves
the file if the original value of :no-save is not t and
`org-note-abort' is not t."
  (let* ((name-templ (or (org-roam-capture--get :file-name)
                         org-roam-capture--file-name-default))
         (new-id (s-trim (org-roam-capture--fill-template
                          name-templ
                          org-roam-capture--info)))
         (file-path (org-roam--file-path-from-id new-id))
         (roam-head (or (org-roam-capture--get :head)
                        org-roam-capture--header-default))
         (org-template (org-capture-get :template))
         (roam-template (concat roam-head org-template)))
    (unless (file-exists-p file-path)
      (make-directory (file-name-directory file-path) t)
      (org-roam-capture--put :orig-no-save (org-capture-get :no-save)
                             :new-file t)
      (org-capture-put :template
                       ;; Fixes org-capture-place-plain-text throwing 'invalid search bound'
                       ;; when both :unnarowed t and "%?" is missing from the template string;
                       ;; may become unnecessary when the upstream bug is fixed
                       (if (s-contains-p "%?" roam-template)
                           roam-template
                         (concat roam-template "%?"))
                       :type 'plain
                       :no-save t))
    file-path))

(defun org-roam-capture--expand-template ()
  "Expand capture template with information from `org-roam-capture--info'."
  (org-capture-put :template
                   (s-format (org-capture-get :template)
                             (lambda (key)
                               (or (s--aget org-roam-capture--info key)
                                   (when-let ((v (completing-read (format "%s: " key ) nil)))
                                     (push (cons key v) org-roam-capture--info)
                                     v))) nil)))

(defun org-roam-capture--get-point ()
  "Return exact point to file for org-capture-template.
The file to use is dependent on the context:

If the search is via title, it is assumed that the file does not
yet exist, and Org-roam will attempt to create new file.

If the search is via daily notes, 'time will be passed via
`org-roam-capture--info'. This is used to alter the default time
in `org-capture-templates'.

If the search is via ref, it is matched against the Org-roam database.
If there is no file with that ref, a file with that ref is created.

This function is used solely in Org-roam's capture templates: see
`org-roam-capture-templates'."
  (let ((file-path (pcase org-roam-capture--context
                     ('capture
                      (or (cdr (assoc 'file org-roam-capture--info))
                          (org-roam-capture--new-file)))
                     ('title
                      (org-roam-capture--new-file))
                     ('dailies
                      (org-capture-put :default-time (cdr (assoc 'time org-roam-capture--info)))
                      (org-roam-capture--new-file))
                     ('ref
                      (let ((completions (org-roam--get-ref-path-completions))
                            (ref (cdr (assoc 'ref org-roam-capture--info))))
                        (if-let ((pl (cdr (assoc ref completions))))
                            (plist-get pl :path)
                          (org-roam-capture--new-file))))
                     (_ (error "Invalid org-roam-capture-context")))))
    (org-roam-capture--expand-template)
    (org-roam-capture--put :file-path file-path)
    (while org-roam-capture-additional-template-props
      (let ((prop (pop org-roam-capture-additional-template-props))
            (val (pop org-roam-capture-additional-template-props)))
        (org-roam-capture--put prop val)))
    (set-buffer (org-capture-target-buffer file-path))
    (widen)
    (goto-char (point-max))))

(defun org-roam-capture--convert-template (template)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax."
  (pcase template
    (`(,_key ,_description) template)
    (`(,key ,description ,type ,target . ,rest)
     (let ((converted `(,key ,description ,type ,target
                             ,(unless (keywordp (car rest)) (pop rest))))
           org-roam-plist
           options)
       (while rest
         (let* ((key (pop rest))
                (val (pop rest))
                (custom (member key org-roam-capture--template-keywords)))
           (push val (if custom org-roam-plist options))
           (push key (if custom org-roam-plist options))))
       (append converted options `(:org-roam ,org-roam-plist))))
    (_ (user-error "Invalid capture template format: %s" template))))

(defun org-roam-capture--find-file-h ()
  "Opens the newly created template file.
This is added as a hook to `org-capture-after-finalize-hook'.
Run the hooks defined in `org-roam-capture-after-find-file-hook'."
  (unless org-note-abort
    (when-let ((file-path (org-roam-capture--get :file-path)))
      (find-file file-path))
    (run-hooks 'org-roam-capture-after-find-file-hook))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h))

(defcustom org-roam-capture-after-find-file-hook nil
  "Hook that is run right after an Org-roam capture process is finalized.
Suitable for moving point."
  :group 'org-roam
  :type 'hook)

(defun org-roam-capture--capture (&optional goto keys)
  "Create a new file, and return the path to the edited file.
The templates are defined at `org-roam-capture-templates'.  The
GOTO and KEYS argument have the same functionality as
`org-capture'."
  (let ((org-capture-templates (mapcar #'org-roam-capture--convert-template org-roam-capture-templates))
        org-capture-templates-contexts)
    (when (= (length org-capture-templates) 1)
      (setq keys (caar org-capture-templates)))
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--save-file-maybe-h)
    (org-capture goto keys)))

;;;###autoload
(defun org-roam-capture ()
  "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (when (org-roam-capture--in-process-p)
    (user-error "Nested Org-roam capture processes not supported"))
  (let* ((completions (org-roam--get-title-path-completions))
         (title-with-keys (org-roam-completion--completing-read "File: "
                                                                completions))
         (res (cdr (assoc title-with-keys completions)))
         (title (or (plist-get res :title) title-with-keys))
         (file-path (plist-get res :path)))
    (let ((org-roam-capture--info (list (cons 'title title)
                                        (cons 'slug (org-roam--title-to-slug title))
                                        (cons 'file file-path)))
          (org-roam-capture--context 'capture))
      (setq org-roam-capture-additional-template-props (list :capture-fn 'org-roam-capture))
      (condition-case err
          (org-roam-capture--capture)
        (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                           (error-message-string err)))))))

(provide 'org-roam-capture)

;;; org-roam-capture.el ends here
