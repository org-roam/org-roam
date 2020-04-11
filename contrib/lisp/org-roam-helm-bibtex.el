;;; org-roam-helm-bibtex.el --- Org-roam helm-bibtex integration  -*- fill-column: 78; lexical-binding: t; -*-

;; Authors: Leo Vivier <leo.vivier+dev@gmail.com>

;;; Commentary:

;; This script provides basic interaction between `org-roam' and
;; `helm-bibtex' by enabling bibliographic notes stored in
;; `org-roam-directory' to be accessed from `helm-bibtex'.  If a note does not
;; exist, the user is asked to create one.

;;; Code:

(require 'helm-bibtex)

(defun org-roam-bibtex-completion-edit-notes (keys)
  "Open the notes associated with the selected entries using `find-file'."
  (dolist (key keys)
    (let ((refs (org-roam--get-ref-path-completions)))
      (if-let ((path (cdr (assoc key refs))))
          (find-file path)
        (when (y-or-n-p (format "No note was found for %s.  Would you like to create one?" key))
          (let* ((title key)
                 (org-roam-capture--info (list (cons 'title title)
                                               (cons 'ref (format "%s" key))
                                               (cons 'slug (org-roam--title-to-slug key))))
                 (org-roam-capture--context 'ref)
                 (org-roam-capture-templates org-roam-capture-ref-templates))
            (org-roam--capture)))))))

(advice-add 'bibtex-completion-edit-notes :override #'org-roam-bibtex-completion-edit-notes)

(provide 'org-roam-helm-bibtex)
;;; org-roam-helm-bibtex.el ends here
