;; Introduced in Emacs 27.1
(unless (fboundp 'make-empty-file)
  (defun make-empty-file (filename &optional parents)
    "Create an empty file FILENAME.
Optional arg PARENTS, if non-nil then creates parent dirs as needed.

If called interactively, then PARENTS is non-nil."
    (interactive
     (let ((filename (read-file-name "Create empty file: ")))
       (list filename t)))
    (when (and (file-exists-p filename) (null parents))
      (signal 'file-already-exists `("File exists" ,filename)))
    (let ((paren-dir (file-name-directory filename)))
      (when (and paren-dir (not (file-exists-p paren-dir)))
        (make-directory paren-dir parents)))
    (write-region "" nil filename nil 0)))

(provide'org-roam-polyfill)
