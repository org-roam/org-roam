(require 'deft)

(defcustom org-roam-zettel-indicator "§"
  "Indicator in front of a zettel.")

(defun org-roam-insert (file-name)
  "Finds a file, inserts it as a link with the base file name as the link name, and adds the zd-link-indicator I use to the front."
  (interactive (list (completing-read "File: " (deft-find-all-files-no-prefix))))
  (let ((org-link-file-type 'relative))
    (org-insert-link nil (concat "file:" (concat deft-directory file-name))
                     (concat org-roam-zettel-indicator (file-name-base file-name)))))

(defun org-roam-get-linked-files ()
  "Show links to this file."
  (interactive)
  (let* ((search-term (file-name-nondirectory buffer-file-name))
         (files deft-all-files)
	       (tnames (mapcar #'file-truename files)))
    (multi-occur
     (mapcar (lambda (x)
	             (with-current-buffer
		               (or (get-file-buffer x) (find-file-noselect x))
		             (widen)
		             (current-buffer)))
	           files)
     search-term
     3)))

(provide 'org-roam)
