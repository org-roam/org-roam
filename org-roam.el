;;; org-roam.el --- Roam Research replica with Org-mode and Deft

;;; Commentary:
;;

;;; Code:
(require 'deft)
(require 'org-element)
(require 'async)
(require 'subr-x)
(require 's)

(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-")

(defvar org-roam-directory deft-directory
  "Org roam directory.")

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

(defvar org-roam-buffer "*org-roam*"
  "Org-roam buffer name.")

(defvar org-roam-preview-content-delimiter "------"
  "Delimiter for preview content.")

(defvar org-roam-cache nil
  "Cache containing backlinks for `org-roam' buffers.")

(defvar org-roam-current-file-id nil
  "Currently displayed file in `org-roam' buffer.")

(defvar org-roam-update-interval 5
  "Number of minutes to run asynchronous update of backlinks.")

(defvar org-roam-update-timer nil
  "Variable containing the timer that periodically updates the buffer.")

(defvar org-roam-graph-viewer (executable-find "firefox")
  "Path to executable for viewing SVG.")

(defvar org-roam-graphviz-executable (executable-find "dot")
  "Path to graphviz executable.")

(define-inline org-roam-current-visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam--find-files (dir)
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            (dir-ignore-regexp (concat "\\(?:"
                                       "\\."
                                       "\\|\\.\\."
                                       "\\)$"))
            result)
        (dolist (file files)
          (cond
           ((file-directory-p file)
            (when (not (string-match dir-ignore-regexp file))
              (setq result (append (org-roam--find-files file) result))))
           ((and (file-readable-p file)
                 (string= (file-name-extension file) "org"))
            (setq result (cons file result)))))
        result)))

(defun org-roam--find-all-files ()
  (org-roam--find-files org-roam-directory))

(defun org-roam--get-file-path-absolute (id)
  "Converts identifier `ID' to the absolute file path."
  (expand-file-name
   (concat id ".org")
   (file-truename org-roam-directory)))

(defun org-roam--get-file-path (id)
  "Converts identifier `ID' to the relative file path."
  (file-relative-name (org-roam--get-file-path-absolute id)))

(defun org-roam--get-id (file-path)
  (file-name-sans-extension
   (file-relative-name
    (file-truename file-path)
    (file-truename org-roam-directory))))

(defun org-roam-insert (id)
  "Find file `FILE-NAME', insert it as a link with the base file name as the link name."
  (interactive (list (completing-read "File: "
                                      (mapcar #'org-roam--get-id
                                              (org-roam--find-all-files)))))
  (let ((file-path (org-roam--get-file-path id)))
    (insert (format "[[%s][%s]]"
                    (concat "file:" file-path)
                    (concat org-roam-zettel-indicator id)))))

(defun org-roam--build-cache-async ()
  "Builds the cache asychronously, saving it into `org-roam-cache'."
  (interactive)
  (setq org-roam-files (org-roam--find-all-files))
  (setq org-roam-directory deft-directory)
  (async-start
   `(lambda ()
      (require 'org)
      (require 'org-element)
      (require 'subr-x)                 ; temp-fix
      (require 'cl-lib)
      ,(async-inject-variables "org-roam-files")
      ,(async-inject-variables "org-roam-directory")
      (let ((backlinks (make-hash-table :test #'equal)))
        (cl-flet* ((org-roam--get-id (file-path) (file-name-sans-extension
                                                  (file-relative-name
                                                   file-path
                                                   org-roam-directory)))
                   (org-roam--parse-content (file) (with-temp-buffer
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
                                                                 (list file
                                                                       (expand-file-name path org-roam-directory)
                                                                       (string-trim content))))))))))
                   (org-roam--build-backlinks (items) (mapcar
                                                       (lambda (item)
                                                         (pcase-let ((`(,file ,path ,content) item))
                                                           (let* ((link-id (org-roam--get-id path))
                                                                  (backlink-id (org-roam--get-id file))
                                                                  (contents-hash (gethash link-id backlinks)))
                                                             (if contents-hash
                                                                 (if-let ((contents-list (gethash backlink-id contents-hash)))
                                                                     (let ((updated (cons content contents-list)))
                                                                       (puthash backlink-id updated contents-hash)
                                                                       (puthash link-id contents-hash backlinks))
                                                                   (puthash backlink-id (list content) contents-hash)
                                                                   (puthash link-id contents-hash backlinks))
                                                               (let ((contents-hash (make-hash-table :test #'equal)))
                                                                 (puthash backlink-id (list content) contents-hash)
                                                                 (puthash link-id contents-hash backlinks))))))
                                                       items)))
          (mapcar #'org-roam--build-backlinks
                  (mapcar #'org-roam--parse-content org-roam-files)))
        (prin1-to-string backlinks)))
   (lambda (backlinks)
     (setq org-roam-cache (car (read-from-string
                                backlinks)))
     (org-roam--maybe-update-buffer))))

(defun org-roam-new-file-named (slug)
  "Create a new file named `SLUG'.
`SLUG' is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (find-file (org-roam--get-file-path slug)))

(defun org-roam-today ()
  "Create the file for today."
  (interactive)
  (org-roam-new-file-named (format-time-string "%Y-%m-%d" (current-time))))

(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-roam-update (link-id)
  "Show the backlinks for given org file `FILE'."
  (when org-roam-cache
    (let ((title (or (org-element-property :value (car (org-global-props "TITLE")))
                     link-id)))
      (with-current-buffer org-roam-buffer
        (let ((inhibit-read-only t)
              (file-path (org-roam--get-file-path-absolute link-id)))
          (erase-buffer)
          (when (not (eq major-mode 'org-mode))
            (org-mode))
          (make-local-variable 'org-return-follows-link)
          (setq org-return-follows-link t)
          (insert title)
          (insert "\n\n* Backlinks\n")
          (when-let (backlinks (gethash link-id org-roam-cache))
            (maphash (lambda (backlink-id contents)
                       (insert (format "** [[file:%s][%s]]\n" (org-roam--get-file-path backlink-id) backlink-id))
                       (dolist (content contents)
                         (insert (format "%s\n" org-roam-preview-content-delimiter))
                         (insert (s-replace "\n" " " content))
                         (insert (format "\n%s\n\n" org-roam-preview-content-delimiter))))
                     backlinks)))
        (read-only-mode 1)))
    (setq org-roam-current-file-id link-id)))

(defun org-roam ()
  "Initialize `org-roam'.
1. Setup to auto-update `org-roam-buffer' with the correct information.
2. Starts the timer to asynchronously build backlinks.
3. Pops up the window `org-roam-buffer' accordingly."
  (interactive)
  (pcase (org-roam-current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam--setup-buffer))
    ('none (org-roam--setup-buffer))))

(defun org-roam--enable ()
  (add-hook 'post-command-hook #'org-roam--maybe-update-buffer -100 t)
  (setq org-roam-update-timer
        (run-with-timer 0 (* org-roam-update-interval 60) 'org-roam--build-cache-async))
  (org-roam--maybe-update-buffer))

(defun org-roam--disable ()
  (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer)
  (cancel-timer org-roam-update-timer))

(defun org-roam--setup-buffer ()
  "Setup the `org-roam' buffer at the `org-roam-position'."
  (display-buffer-in-side-window
   (get-buffer-create org-roam-buffer)
   `((side . ,org-roam-position))))

(defun org-roam--maybe-update-buffer ()
  "Update `org-roam-buffer' with the necessary information.
This needs to be quick/infrequent, because this is run at
`post-command-hook'. This is achieved by only checking Org files
that are amongst deft files, and `org-roam' not already
displaying information for the correct file."
  (with-current-buffer (window-buffer)
    (when (and (eq major-mode 'org-mode)
               (get-buffer org-roam-buffer)
               (not (string= org-roam-current-file-id (org-roam--get-id (file-truename (buffer-file-name (current-buffer))))))
               (member (file-truename (buffer-file-name (current-buffer))) (org-roam--find-all-files)))
      (org-roam-update (org-roam--get-id (buffer-file-name (current-buffer)))))))

(defun org-roam-build-graph ()
  "Build graphviz graph output."
  (with-temp-buffer
    (insert "digraph {\n")
    (mapcar (lambda (file)
              (insert
               (format "  \"%s\" [URL=\"roam://%s\"];\n"
                       (file-name-nondirectory (file-name-sans-extension file))
                       file)))
            (org-roam--find-all-files))
    (maphash
     (lambda (link-id backlinks)
       (maphash
        (lambda (backlink-id content)
          (insert (format "  %s -> %s;\n" backlink-id link-id)))
        backlinks))
     org-roam-cache)
    (insert "}")
    (buffer-string)))

(defun org-roam-show-graph (&rest body)
  (interactive)
  (unless org-roam-graphviz-executable
    (setq org-roam-graphviz-executable (executable-find "dot")))
  (unless org-roam-graphviz-executable
    (user-error "Can't find graphviz executable. Please check if it is in your path"))
  (declare (indent 0))
  (let ((temp-dot (expand-file-name "graph.dot" temporary-file-directory))
        (temp-graph (expand-file-name "graph.svg" temporary-file-directory))
        (graph (org-roam-build-graph)))
    (with-temp-file temp-dot
      (insert graph))
    (call-process org-roam-graphviz-executable nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (call-process org-roam-graph-viewer nil 0 nil temp-graph)))

(define-minor-mode org-roam-mode
  "Global minor mode to automatically update the org-roam buffer."
  :require 'org-roam
  (if org-roam-mode
      (org-roam--enable)
    (org-roam--disable)))

(provide 'org-roam)

;;; org-roam.el ends here
