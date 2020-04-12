;;; org-roam-helm-bibtex.el --- Org-roam helm-bibtex integration  -*- fill-column: 78; lexical-binding: t; -*-

;; Authors: Leo Vivier <leo.vivier+dev@gmail.com>

;;; Commentary:

;; This script provides basic interaction between `org-roam' and `helm-bibtex'
;; by enabling bibliographic notes stored in `org-roam-directory' to be
;; accessed from `helm-bibtex' or `ivy-bibtex'.  If a note does not exist, the
;; script creates one.

;;; Code:

(require 'org-roam)
(require 'bibtex)
(require 'bibtex-completion)

(with-eval-after-load 'helm
  (require 'helm-bibtex))

(with-eval-after-load 'ivy
  (require 'ivy-bibtex))

(defun org-roam-bibtex-completion-edit-notes (keys)
  "Open the notes associated with the selected entries using `find-file'."
  (dolist (key keys)
    (let ((refs (org-roam--get-ref-path-completions))
          path)
      (if (setq path (cdr (assoc key refs)))
          (find-file path)
        (let* ((title key)
               (org-roam-capture--info (list (cons 'title title)
                                             (cons 'ref (format "cite:%s" key))
                                             (cons 'slug (org-roam--title-to-slug key))))
               (org-roam-capture--context 'ref)
               (org-roam-capture-templates org-roam-capture-ref-templates))
          (org-roam--capture))))))

(advice-add 'bibtex-completion-edit-notes :override #'org-roam-bibtex-completion-edit-notes)

(provide 'org-roam-helm-bibtex)
;;; org-roam-helm-bibtex.el ends here
