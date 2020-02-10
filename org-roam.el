;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'dash)
(require 'org-element)
(require 'async)
(require 'subr-x)
(require 's)
(require 'f)

;;; Customizations
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-")

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Org-roam directory."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-zettel-indicator "§"
  "Indicator in front of a zettel."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-position 'right
  "Position of `org-roam' buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group 'org-roam)

(defcustom org-roam-link-representation 'id
  "The value used to represent an org-roam link.

Valid values are
 * file,
 * title."
  :type '(choice (const id)
                 (const title))
  :group 'org-roam)

(defcustom org-roam-buffer-width 0.33 "Width of `org-roam' buffer."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer "*org-roam*"
  "Org-roam buffer name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Path to executable for viewing SVG."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graphviz-executable (executable-find "dot")
  "Path to graphviz executable."
  :type 'string
  :group 'org-roam)

;;; Polyfills
;; These are for functions I use that are only available in newer Emacs

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

;;; Dynamic variables
(defvar org-roam-cache nil
  "Cache containing backlinks for `org-roam' buffers.")

(defvar org-roam-current-file nil
  "Currently displayed file in `org-roam' buffer.")

;;; Utilities
(defun org-roam--org-roam-file-p ()
  "Return t if file is part of org-roam system, false otherwise."
  (and (buffer-file-name (current-buffer))
       (f-child-of-p (file-truename (buffer-file-name (current-buffer)))
                     org-roam-directory)))

(defun org-roam--get-title (file)
  "Return title of `FILE'.

It first tries the cache. If the cache does not contain the file,
it will return the title by loading the file."
  (if-let ((titles-cache (plist-get org-roam-cache :titles)))
      (or (gethash file titles-cache)
          (org-roam--extract-file-title file))
    (org-roam--extract-file-title file)))

(defun org-roam--find-files (dir)
  "Return all org-roam files in `DIR'."
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
            (setq result (cons (file-truename file) result)))))
        result)))

(defun org-roam--find-all-files ()
  "Return all org-roam files."
  (org-roam--find-files org-roam-directory))

(defun org-roam--get-file-path (id &optional absolute)
  "Convert identifier `ID' to file path.

If `ABSOLUTE', return the absolute file-path. Else, return the relative file-path."
  (let ((absolute-file-path (expand-file-name
                             (concat id ".org")
                             (file-truename org-roam-directory))))
    (if absolute
        absolute-file-path
      (file-relative-name absolute-file-path org-roam-directory))))

(defun org-roam--get-id (file-path)
  "Convert `FILE-PATH' to the org-roam id."
  (file-name-sans-extension
   (file-relative-name
    (file-truename file-path)
    (file-truename org-roam-directory))))

(defun org-roam--get-title-or-id (file-path)
  "Convert `FILE-PATH' to the file title, if it exists. Else, return the id."
  (or (org-roam--get-title file-path)
      (org-roam--get-id file-path)))

;;; Inserting org-roam links
(defun org-roam-insert ()
  "Insert an org-roam link."
  (interactive)
  (pcase org-roam-link-representation
    ('id (org-roam--insert-id))
    ('title (org-roam--insert-title))))

(defun org-roam--insert-title ()
  "Find `ID', and insert a relative org link to it at point."
  (let* ((completions (mapcar (lambda (file)
                                (list (org-roam--get-title-or-id file)
                                      (org-roam--get-id file)))
                              (org-roam--find-all-files)))
         (title (completing-read "File: " completions))
         (id (cadr (assoc title completions))))
    (unless id
      (setq id (read-string "Enter new file id: ")))
    (let ((file-path (org-roam--get-file-path id)))
      (unless (file-exists-p file-path)
        (make-empty-file file-path))
      (insert (format "[[%s][%s]]"
                      (concat "file:" file-path)
                      title)))))

(defun org-roam--insert-id ()
  "Find `ID', and insert a relative org link to it at point."
  (let* ((id (completing-read "File: " (mapcar #'org-roam--get-id (org-roam--find-all-files))))
         (file-path (org-roam--get-file-path id)))
    (unless (file-exists-p file-path)
      (make-empty-file file-path))
    (insert (format "[[%s][%s]]"
                    (concat "file:" file-path)
                    (concat org-roam-zettel-indicator id)))))

;;; Finding org-roam files
(defun org-roam-find-file (id)
  "Find and open file with id `ID'."
  (interactive (list (completing-read "File: "
                                      (mapcar #'org-roam--get-id
                                              (org-roam--find-all-files)))))
  (let ((file-path (org-roam--get-file-path id t)))
    (unless (file-exists-p file-path)
      (make-empty-file file-path))
    (find-file file-path)))

;;; Building the org-roam cache (asynchronously)
(defun org-roam--build-cache-async ()
  "Builds the cache asychronously, saving it into `org-roam-cache'."
  (interactive)
  (setq org-roam-files (org-roam--find-all-files))
  (async-start
   `(lambda ()
      (require 'org)
      (require 'org-element)
      (require 'subr-x)                 ; temp-fix
      (require 'cl-lib)
      ,(async-inject-variables "org-roam-files")
      ,(async-inject-variables "org-roam-directory")
      (let ((backward-links (make-hash-table :test #'equal))
            (forward-links (make-hash-table :test #'equal))
            (file-titles (make-hash-table :test #'equal)))
        (cl-flet* ((org-roam--parse-content
                    (file)
                    (with-temp-buffer
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
                                       (content (or (org-element-property :raw-value element)
                                                    (buffer-substring
                                                     (or (org-element-property :content-begin element)
                                                         (org-element-property :begin element))
                                                     (or (org-element-property :content-end element)
                                                         (org-element-property :end element))))))
                                  (list :from file
                                        :to (file-truename (expand-file-name path org-roam-directory))
                                        :content (string-trim content))))))))))
                   (org-roam--process-items
                    (items)
                    (mapcar
                     (lambda (item)
                       (pcase-let ((`(:from ,p-from :to ,p-to :content ,content) item))
                         ;; Build forward-links
                         (let ((links (gethash p-from forward-links)))
                           (if links
                               (puthash p-from
                                        (if (member p-to links)
                                            links
                                          (cons p-to links)) forward-links)
                             (puthash p-from (list p-to) forward-links)))
                         ;; Build backward-links
                         (let ((contents-hash (gethash p-to backward-links)))
                           (if contents-hash
                               (if-let ((contents-list (gethash p-from contents-hash)))
                                   (let ((updated (cons content contents-list)))
                                     (puthash p-from updated contents-hash)
                                     (puthash p-to contents-hash backward-links))
                                 (progn
                                   (puthash p-from (list content) contents-hash)
                                   (puthash p-to contents-hash backward-links)))
                             (let ((contents-hash (make-hash-table :test #'equal)))
                               (puthash p-from (list content) contents-hash)
                               (puthash p-to contents-hash backward-links))))))
                     items))
                   (org-roam--extract-title
                    (buffer)
                    (with-current-buffer buffer
                      (org-element-map
                          (org-element-parse-buffer)
                          'keyword
                        (lambda (kw)
                          (when (string= (org-element-property :key kw) "TITLE")
                            (org-element-property :value kw)))
                        :first-match t))))
          (mapcar #'org-roam--process-items
                  (mapcar #'org-roam--parse-content org-roam-files))
          (mapcar (lambda (file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (when-let ((title (org-roam--extract-title (current-buffer))))
                        (puthash file title file-titles))))
                  org-roam-files))
        (list
         :forward forward-links
         :backward backward-links
         :titles file-titles)))
   (lambda (cache)
     (setq org-roam-cache cache)
     (message "Org-roam cache built!"))))

(defun org-roam--insert-item (item)
  "Insert `ITEM' into `org-roam-cache'.

`ITEM' is of the form: (:from from-path :to to-path :content preview-content)

Before calling this function, `org-roam-cache' should be already populated."
  (let ((forward-cache (plist-get org-roam-cache :forward))
        (backward-cache (plist-get org-roam-cache :backward))
        (title-cache (plist-get org-roam-cache :titles)))
    (pcase-let ((`(:from ,p-from :to ,p-to :content ,content) item))
      ;; Build forward-links
      (let ((links (gethash p-from forward-cache)))
        (if links
            (puthash p-from
                     (if (member p-to links)
                         links
                       (cons p-to links)) forward-cache)
          (puthash p-from (list p-to) forward-cache)))
      ;; Build backward-links
      (let ((contents-hash (gethash p-to backward-cache)))
        (if contents-hash
            (if-let ((contents-list (gethash p-from contents-hash)))
                (let ((updated (cons content contents-list)))
                  (puthash p-from updated contents-hash)
                  (puthash p-to contents-hash backward-cache))
              (progn
                (puthash p-from (list content) contents-hash)
                (puthash p-to contents-hash backward-cache)))
          (let ((contents-hash (make-hash-table :test #'equal)))
            (puthash p-from (list content) contents-hash)
            (puthash p-to contents-hash backward-cache))))
      (setq org-roam-cache (list :forward forward-cache
                                 :backward backward-cache
                                 :titles title-cache)))))

(defun org-roam--parse-content ()
  "Parse the current buffer, and return a list of items for processing."
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
                   (content (or (org-element-property :raw-value element)
                                (buffer-substring
                                 (or (org-element-property :content-begin element)
                                     (org-element-property :begin element))
                                 (or (org-element-property :content-end element)
                                     (org-element-property :end element))))))
              (list :from (file-truename (buffer-file-name (current-buffer)))
                    :to (file-truename (expand-file-name path org-roam-directory))
                    :content (string-trim content)))))))))

(defun org-roam--clear-cache-for-buffer (buffer)
  "Remove any related links to the file for `BUFFER'.

This is equivalent to removing the node from the graph."
  (with-current-buffer (current-buffer)
    (let ((file (file-truename (buffer-file-name buffer)))
          (forward-cache (plist-get org-roam-cache :forward))
          (backward-cache (plist-get org-roam-cache :backward))
          (titles-cache (plist-get org-roam-cache :titles)))
      ;; Step 1: Remove all existing links for file
      (when-let ((forward-links (gethash file forward-cache)))
        ;; Delete backlinks to file
        (dolist (link forward-links)
          (when-let ((backward-links (gethash link backward-cache)))
            (remhash file backward-links)
            (puthash link backward-links backward-cache)))
        ;; Clean out forward links
        (remhash file forward-cache))
      ;; Step 2: Remove from the title cache
      (remhash file titles-cache)
      (setq org-roam-cache (list :forward forward-cache
                                 :backward backward-cache
                                 :titles titles-cache)))))

(defun org-roam--update-cache-title (buffer)
  "Inserts the `TITLE' of file in buffer into the cache."
  (when-let ((titles-cache (plist-get org-roam-cache :titles))
             (title (org-roam--extract-title buffer)))
    (puthash (file-truename (buffer-file-name buffer))
             title
             titles-cache)
    (setq org-roam-cache (plist-put org-roam-cache :titles titles-cache))))

(defun org-roam--update-cache ()
  "Update `org-roam-cache' for the current buffer file."
  (save-excursion
    (org-roam--clear-cache-for-buffer (current-buffer))
    ;; Insert into title cache
    (org-roam--update-cache-title (current-buffer))
    ;; Insert new items
    (let ((items (org-roam--parse-content)))
      (dolist (item items)
        (org-roam--insert-item item)))))

;;; Org-roam daily notes
(defun org-roam--new-file-named (slug)
  "Create a new file named `SLUG'.
`SLUG' is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (find-file (org-roam--get-file-path slug t)))

(defun org-roam-today ()
  "Create the file for today."
  (interactive)
  (org-roam--new-file-named (format-time-string "%Y-%m-%d" (current-time))))

(defun org-roam-tomorrow ()
  "Create the file for tomorrow."
  (interactive)
  (org-roam--new-file-named (format-time-string "%Y-%m-%d" (time-add 86400 (current-time)))))

(defun org-roam-date ()
  "Create the file for any date using the calendar."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (org-roam--new-file-named (format-time-string "%Y-%m-%d" time))))

;;; Org-roam buffer updates
(defun org-roam--extract-title (buffer)
  "Extract the title from `BUFFER'."
  (with-current-buffer buffer
    (org-element-map
        (org-element-parse-buffer)
        'keyword
      (lambda (kw)
        (when (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
      :first-match t)))

(defun org-roam--extract-file-title (file)
  "Extract the title from `FILE'."
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-map
        (org-element-parse-buffer)
        'keyword
      (lambda (kw)
        (when (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
      :first-match t)))

(defun org-roam-update (file-path)
  "Show the backlinks for given org file for file at `FILE-PATH'."
  (when org-roam-cache
    (let ((buffer-title (org-roam--get-title-or-id file-path)))
      (with-current-buffer org-roam-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when (not (eq major-mode 'org-mode))
            (org-mode))
          (make-local-variable 'org-return-follows-link)
          (setq org-return-follows-link t)
          (insert
           (propertize buffer-title 'font-lock-face 'org-document-title))
          (if-let ((backlinks (gethash file-path (plist-get org-roam-cache :backward))))
              (progn
                (insert (format "\n\n* %d Backlinks\n"
                                (hash-table-count backlinks)))
                (maphash (lambda (file-from contents)
                           (insert (format "** [[file:%s][%s]]\n"
                                           file-from
                                           (org-roam--get-title-or-id file-from)))
                           (dolist (content contents)
                             (insert (concat (propertize (s-trim (s-replace "\n" " " content))
                                                         'font-lock-face 'org-block)
                                             "\n\n"))))
                         backlinks))
            (insert "\n\n* No backlinks!")))
        (read-only-mode 1)))
    (setq org-roam-current-file file-path)))

;;; Show/hide the org-roam buffer
(define-inline org-roam--current-visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam--set-width (width)
  "Set the width of the org-roam buffer to `WIDTH'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun org-roam--setup-buffer ()
  "Setup the `org-roam' buffer at the `org-roam-position'."
  (let ((window (get-buffer-window)))
    (-> (get-buffer-create org-roam-buffer)
        (display-buffer-in-side-window
         `((side . ,org-roam-position)))
        (select-window))
    (org-roam--set-width
     (round (* (frame-width)
               org-roam-buffer-width)))
    (select-window window)))

(defun org-roam ()
  "Pops up the window `org-roam-buffer' accordingly."
  (interactive)
  (pcase (org-roam--current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam--setup-buffer))
    ('none (org-roam--setup-buffer))))

;;; The minor mode definition that updates the buffer
(defun org-roam--maybe-enable ()
  "Enable org-roam updating for file, if file is an org-roam file."
  (when (org-roam--org-roam-file-p)
    (org-roam--enable)))

(defun org-roam--enable ()
  "Enable org-roam updating for file.

1. If the cache does not yet exist, build it asynchronously.
2. Setup hooks for updating the cache, and the org-roam buffer."
  (unless org-roam-cache
    (org-roam--build-cache-async))
  (add-hook 'post-command-hook #'org-roam--maybe-update-buffer nil t)
  (add-hook 'after-save-hook #'org-roam--update-cache))

(defun org-roam--disable ()
  "Disable org-roam updating for file.

1. Remove hooks for updating the cache, and the org-roam buffer."
  (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer)
  (remove-hook 'after-save-hook #'org-roam--update-cache))

(defun org-roam--maybe-update-buffer ()
  "Update `org-roam-buffer' with the necessary information.
This needs to be quick/infrequent, because this is run at
`post-command-hook'."
  (with-current-buffer (window-buffer)
    (when (and (get-buffer org-roam-buffer)
               (buffer-file-name (current-buffer))
               (not (string= org-roam-current-file
                             (file-truename (buffer-file-name (current-buffer))))))
      (org-roam-update (file-truename (buffer-file-name (window-buffer)))))))

(define-minor-mode org-roam-mode
  "Global minor mode to automatically update the org-roam buffer."
  :require 'org-roam
  (if org-roam-mode
      (org-roam--maybe-enable)
    (org-roam--disable)))

;;; Building the Graphviz graph
(defun org-roam-build-graph ()
  "Build graphviz graph output."
  (let ((forward-links (plist-get org-roam-cache :forward)))
    (with-temp-buffer
      (insert "digraph {\n")
      (mapcar (lambda (file)
                (insert
                 (format "  \"%s\" [URL=\"roam://%s\"];\n"
                         (org-roam--get-id file)
                         file)))
              (org-roam--find-all-files))
      (maphash
       (lambda (from-link to-links)
         (dolist (to-link to-links)
           (insert (format "  \"%s\" -> \"%s\";\n"
                           (org-roam--get-id from-link)
                           (org-roam--get-id to-link))))
         )
       forward-links)
      (insert "}")
      (buffer-string))))

(defun org-roam-show-graph ()
  "Generate the org-roam graph in SVG format, and display it using `org-roam-graph-viewer'."
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


(provide 'org-roam)

;;; org-roam.el ends here

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
