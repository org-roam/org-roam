;;; org-roam-compat.el --- Backward compatibility code -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1"))

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
;; This file is dedicated to maintain backward compatibility with older older
;; Emacsen and Org-roam versions.
;;
;;; Code:
(require 'org-roam)

;;; Backports
;; REVIEW Remove when 26.x support is dropped. This is exact the same as
;; `directory-files-recursively' from Emacs 26, but with FOLLOW-SYMLINKS
;; parameter from Emacs 27.
(defun org-roam--directory-files-recursively (dir regexp
                                                  &optional include-directories predicate
                                                  follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (condition-case nil
                               (org-roam--directory-files-recursively
                                full-file regexp include-directories
                                predicate follow-symlinks)
                             (file-error nil))
                         (org-roam--directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

;;; Compatibility hacks and patches
(advice-add #'org-id-add-location :around #'org-roam--handle-absent-org-id-locations-file-a)
(defun org-roam--handle-absent-org-id-locations-file-a (fn &rest args)
  "Gracefully handle errors related to absence of `org-id-locations-file'.
FN is `org-id-add-location' that comes from advice and ARGS are
passed to it."
  (condition-case err
      (apply fn args)
    ;; `org-id' makes the assumption that `org-id-locations-file' will be stored in `user-emacs-directory'
    ;; which always exist if you have Emacs, so it uses `with-temp-file' to write to the file. However, the
    ;; users *do* change the path to this file and `with-temp-file' unable to create the file, if the path to
    ;; it consists of directories that don't exist. We'll have to handle this ourselves.
    (error
     (advice-remove 'org-id-add-location #'org-roam--handle-absent-org-id-locations-file-a)
     (if (file-exists-p (file-truename org-id-locations-file))
         (signal (car err) (cdr err))
       ;; Pre-allocate the hash table to avoid weird access related errors during the regeneration.
       (or org-id-locations (setq org-id-locations (make-hash-table :test 'equal)))
       ;; If permissions allow that, try to create the user specified directory path to
       ;; `org-id-locations-file' ourselves.
       (condition-case _err
           (progn (org-roam-message (concat "`org-id-locations-file' (%s) doesn't exist. "
                                            "Trying to regenerate it (this may take a while)...")
                                    org-id-locations-file)
                  (make-directory (file-name-directory (file-truename org-id-locations-file)))
                  (org-roam-update-org-id-locations)
                  (apply fn args))
         ;; In case of failure (lack of permissions), we'll patch it to at least handle the current session
         ;; without errors.
         (file-error (org-roam-message "Failed to regenerate `org-id-locations-file'")
                     (lwarn 'org-roam :error "
--------
WARNING: `org-id-locations-file' (%s) doesn't exist!
         Org-roam is unable to create it for you.
--------

This happens when Emacs doesn't have permissions to create the
path to your `org-id-locations-file'. Org-roam will now fallback
storing the file in your current `org-roam-directory', but the
warning will keep popup with each new session.

To stop this warning from popping up, set `org-id-locations-file'
to the location you want and ensure that the path exists on your
filesystem, then run M-x `org-roam-update-org-id-locations'.

Note: While Org-roam doesn't depend on `org-id-locations-file' to
lookup IDs for the nodes that are stored in the database, it
still tries to keep it updated so IDs work across other files in
Org-mode, so the IDs used in your `org-roam-directory' would be
able to cross-reference outside of `org-roam-directory'. It also
allows to keep linking with \"id:\" links within the current
`org-roam-directory' to headings and files that are excluded from
identification (e.g. with \"ROAM_EXCLUDE\" property) as Org-roam
nodes." org-id-locations-file)
                     (setq org-id-locations-file
                           (expand-file-name ".orgids" (file-truename org-roam-directory)))
                     (apply fn args)))))))

;;;; Deprecated :if-new capture template keyword
(with-eval-after-load 'org-roam-capture
  (add-to-list 'org-roam-capture--template-keywords :if-new)

  (let ((inhibit-warning-p t)) ; REVIEW Set this to nil close to next major release
    (advice-add 'org-roam-capture--get-target :around #'org-roam-capture--get-if-new-target-a)
    (defun org-roam-capture--get-if-new-target-a (fn &rest args)
      "Get the current capture target using deprecated :if-new property."
      (if-let ((target (org-roam-capture--get :if-new)))
          (prog1 target
            (unless inhibit-warning-p
              (lwarn 'org-roam-capture :warning
                     (mapconcat
                      #'identity
                      ["`:if-new' property is deprecated in favor of `:target'."
                       "This warning will popup once per each session. In order to get"
                       "rid of it, rename all the references to the `:if-new' property"
                       "in your capture templates to `:target'."]
                      "\n"))
              ;; Don't irritate the user too much. Displaying the warning once per session should be enough.
              (setq inhibit-warning-p t)))
        (apply fn args)))))

;;; Obsolete aliases (remove after next major release)
(define-obsolete-function-alias
  'org-roam-setup
  'org-roam-db-autosync-enable "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-teardown
  'org-roam-db-autosync-disable "org-roam 2.0")

(define-obsolete-variable-alias
  'org-roam-current-node
  'org-roam-buffer-current-node "org-roam 2.0")
(define-obsolete-variable-alias
  'org-roam-current-directory
  'org-roam-buffer-current-directory "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-buffer-render
  'org-roam-buffer-render-contents "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-buffer
  'org-roam-buffer-display-dedicated "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-visit-thing
  'org-roam-buffer-visit-thing "org-roam 2.0")

(define-obsolete-function-alias
  'org-roam-dailies-find-today
  'org-roam-dailies-goto-today "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-dailies-find-yesterday
  'org-roam-dailies-goto-yesterday "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-dailies-find-tomorrow
  'org-roam-dailies-goto-tomorrow "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-dailies-find-next-note
  'org-roam-dailies-goto-next-note "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-dailies-find-previous-note
  'org-roam-dailies-goto-previous-note "org-roam 2.0")
(define-obsolete-function-alias
  'org-roam-dailies-find-date
  'org-roam-dailies-goto-date "org-roam 2.0")

(define-obsolete-function-alias
  'org-roam-add-property
  'org-roam-property-add "org-roam 2.1")

(define-obsolete-function-alias
  'org-roam-remove-property
  'org-roam-property-remove "org-roam 2.1")

(define-obsolete-variable-alias
  'org-roam-mode-section-functions
  'org-roam-mode-sections "org-roam 2.2.0")

;;; Obsolete functions
(make-obsolete 'org-roam-get-keyword 'org-collect-keywords "org-roam 2.0")

(provide 'org-roam-compat)

;;; org-roam-compat.el ends here
