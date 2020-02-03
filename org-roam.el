;;; org-roam.el --- Roam Research replica with Org-mode and Deft

;;; Commentary:
;;

;;; Code:
(require 'deft)
(require 'async)
(require 'subr-x)

(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-")

(defcustom org-roam-zettel-indicator "§"
  "Indicator in front of a zettel.")

(defcustom org-roam-position 'right
  "Position of `org-roam' buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'org-roam)

(defvar org-roam-directory nil
  "Org-roam directory. Typically set it to your deft directory.")

(defvar org-roam-buffer "*org-roam*"
  "Org-roam buffer name.")

(defvar org-roam-hash-backlinks nil
  "Cache containing backlinks for `org-roam' buffers.")

(defvar org-roam-current-file nil
  "Currently displayed file in `org-roam' buffer.")

(defvar org-roam-update-interval 5
  "Number of minutes to run asynchronous update of backlinks.")

(defvar org-roam-update-timer nil
  "Variable containing the timer that periodically updates the buffer.")

(define-inline org-roam-current-visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam-insert (file-name)
  "Find file `FILE-NAME', insert it as a link with the base file name as the link name."
  (interactive (list (completing-read "File: "
                                      (mapcar (lambda (f)
                                                (file-name-sans-extension f))
                                              (deft-find-all-files-no-prefix)))))
  (let* ((org-link-file-type 'relative)
         (file-name (concat file-name ".org"))
         (file-path (file-relative-name (file-truename (concat deft-directory file-name)))))
    (org-insert-link nil (concat "file:" file-path)
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

(defun org-roam-get-links-from-buffer (buffer)
  "Return a list of links from BUFFER."
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (string= type "file")
                     (string= (file-name-extension path) "org"))
            path))))))

(defun org-roam-add-backlink (hash link_a link_b)
  "Add a backlink LINK_A <- LINK_B to hash-table HASH."
  (let* ((item (gethash link_a hash))
         (updated (if item
                      (if (member link_b item)
                          item
                        (cons link_b item))
                    (list link_b))))
    (puthash link_a updated hash)))

(defun org-roam-build-backlinks-async ()
  "Builds the backlink hash table asychronously, saving it into `org-roam-hash-backlinks'."
  (interactive)
  (setq org-roam-files (deft-find-all-files))
  (async-start
   `(lambda ()
      (require 'org)
      (require 'org-element)
      (require 'subr-x)                 ; temp-fix
      ,(async-inject-variables "org-roam-")
      (let ((backlinks (make-hash-table :test #'equal)))
        (mapcar (lambda (file)
                  (let ((items (with-temp-buffer
                                 (insert-file-contents file)
                                 (with-current-buffer (current-buffer)
                                   (org-element-map (org-element-parse-buffer) 'link
                                     (lambda (link)
                                       (let ((type (org-element-property :type link))
                                             (path (org-element-property :path link))
                                             (start (org-element-property :begin link)))
                                         (when (and (string= type "file")
                                                    (string= (file-name-extension path) "org"))
                                           (goto-char start)
                                           (let* ((element (org-element-at-point))
                                                  (content (buffer-substring
                                                            (or (org-element-property :content-begin element)
                                                                (org-element-property :begin element))
                                                            (or (org-element-property :content-end element)
                                                                (org-element-property :end element)))))
                                             (list path (string-trim content)))))))))))
                    (mapcar (lambda (item)
                              (let* ((path (car item))
                                     (content (cadr item))
                                     (relative-file (file-name-nondirectory file))
                                     (contents-hash (gethash path backlinks)))
                                (if contents-hash
                                    (if-let ((contents-list (gethash relative-file contents-hash)))
                                        (let ((updated (append content contents-list)))
                                          (puthash relative-file updated contents-hash)
                                          (puthash path contents-hash backlinks))
                                      (puthash relative-file (list content) contents-hash)
                                      (puthash path contents-hash backlinks))
                                  (let ((contents-hash (make-hash-table :test #'equal)))
                                    (puthash relative-file (list content) contents-hash)
                                    (puthash path contents-hash backlinks)))))
                            items)))
                org-roam-files)
        (prin1-to-string backlinks)))
   (lambda (backlinks)
     (setq org-roam-hash-backlinks (car (read-from-string
                                         backlinks)))
     (org-roam-update-buffer))))

(defun org-roam-new-file-named (slug)
  "Create a new file named `SLUG'.
`SLUG' is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (let ((file (deft-absolute-filename slug)))
    (unless (file-exists-p file)
      (deft-auto-populate-title-maybe file)
      (deft-cache-update-file file)
      (deft-refresh-filter))
    (deft-open-file file)))

(defun org-roam-today ()
  "Create the file for today."
  (interactive)
  (org-roam-new-file-named (format-time-string "%Y-%m-%d" (current-time))))

(defun org-roam-update (file)
  "Show the backlinks for given org file `FILE'."
  (when org-roam-hash-backlinks
    (let ((backlinks (gethash file org-roam-hash-backlinks)))
      (with-current-buffer org-roam-buffer
        (read-only-mode -1)
        (erase-buffer)
        (org-mode)
        (make-local-variable 'org-return-follows-link)
        (setq org-return-follows-link t)
        (insert (format "Backlinks for %s:\n\n" file))
        (if backlinks
            (maphash (lambda (link contents)
                       (insert (format "* [[file:%s][%s]]\n" (expand-file-name link org-roam-directory) link))
                       (dolist (content contents)
                         (insert "#+BEGIN_SRC org\n")
                         (insert content)
                         (insert "\n#+END_SRC\n\n")))
                     backlinks)
          (insert "No backlinks."))
        (org-global-cycle 1))
      (setq org-roam-current-file file))))

(defun org-roam ()
  "Initialize `org-roam'.
1. Setup to auto-update `org-roam-buffer' with the correct information.
2. Starts the timer to asynchronously build backlinks.
3. Pops up the window `org-roam-buffer' accordingly."
  (interactive)
  (add-hook 'post-command-hook 'org-roam-update-buffer)
  (unless org-roam-update-timer
    (setq org-roam-update-timer
          (run-with-timer 0 (* org-roam-update-interval 60) #'org-roam-build-backlinks-async)))
  (pcase (org-roam-current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam-setup-buffer))
    ('none (org-roam-setup-buffer))))

(defun org-roam-stop ()
  "Cancels auto-building of backlinks."
  (interactive)
  (remove-hook 'post-command-hook 'org-roam-update-buffer)
  (when org-roam-update-timer
    (cancel-timer org-roam-update-timer)
    (setq org-roam-update-timer nil)))

(defun org-roam-setup-buffer ()
  "Setup the `org-roam' buffer at the `org-roam-position'."
  (display-buffer-in-side-window
   (get-buffer-create org-roam-buffer)
   `((side . ,org-roam-position))))

(defun org-roam-update-buffer ()
  "Update `org-roam-buffer' with the necessary information.
This needs to be quick/infrequent, because this is run at
`post-command-hook'. This is achieved by only checking Org files
that are amongst deft files, and `org-roam' not already
displaying information for the correct file."
  (when (and (eq major-mode 'org-mode)
             (not (string= org-roam-current-file (buffer-file-name (current-buffer))))
             (member (buffer-file-name (current-buffer)) deft-all-files))
    (org-roam-update (file-name-nondirectory (buffer-file-name (current-buffer))))))

(defun org-roam-build-graph ()
  "Build graphviz graph output."
  (with-temp-buffer
    (insert "digraph {\n")
    (maphash
     (lambda (link backlinks)
       (maphash
        (lambda (backlink content)
          (insert (format "  %s -> %s;\n" (file-name-sans-extension link) (file-name-sans-extension backlink))))
        backlinks))
     org-roam-hash-backlinks)
    (insert "}")
    (buffer-string)))

(defun org-roam-show-graph (&rest body)
  (interactive)
  (declare (indent 0))
  (let ((buffer (get-buffer-create "*org-roam-graph*"))
        (temp-dot (expand-file-name "graph.dot" temporary-file-directory))
        (temp-graph (expand-file-name "graph.png" temporary-file-directory))
        (graph (org-roam-build-graph)))
    (with-temp-file temp-dot
      (insert graph))
    (shell-command (format "dot %s -Tpng -o %s" temp-dot temp-graph))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image-file temp-graph)
        (image-mode)))
    (switch-to-buffer-other-window buffer)))

(provide 'org-roam)

;;; org-roam.el ends here
