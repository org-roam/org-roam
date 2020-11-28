;; For the lack of a better name, we're now calling this file org-roam-widget,
;; but it is meant to replace org-roam-buffer at some point

(require 'eieio)
(require 'magit-section)

;; Faces
(defface org-roam-title
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

;;; Widget Class Definition
(cl-deftype function ()
  '(satisfies fboundp))

(defclass org-roam-widget ()
  ((name :initarg :name
         :type symbol
         :custom symbol
         :documentation "The symbol for the widget.")
   (header :initarg :header
           :initform ""
           :type string
           :custom string
           :documentation "The header used for this widget")
   (items :initarg :items
          :type function
          :custom function
          :documentation "The function to call to obtain the items.
Items are of the form: ((key (list of values for key)))")
   (render :initarg :render
           :type function
           :custom function
           :documentation "The function used to render the items.")
   (show-p :initarg :show-p
           :type function
           :custom function
           :documentation "The predicate function to use to see if the widget should be used.")))

(cl-defmethod org-roam-widget-show ((widget org-roam-widget))
  "Return t if WIDGET is to be shown, nil otherwise."
  (funcall (oref widget show-p)))

(cl-defmethod org-roam-widget-render ((widget org-roam-widget))
  "Render items in WIDGET."
  (magit-insert-section (widget-root)
    (magit-insert-heading (oref widget header))
    (if-let* ((items (funcall (oref widget items)))
              (not-empty (not (funcall (oref widget render) items))))
        (insert "\n")
      (magit-cancel-section))))

;;; Widgets
;;;; Backlinks Widget
(defun org-roam-widget-get-backlinks ()
  (let* ((file-path (buffer-file-name org-roam-buffer--current))
         (titles (with-current-buffer org-roam-buffer--current
                   (org-roam--extract-titles)))
         (backlinks (org-roam--get-backlinks (push file-path titles))))
    (--group-by (nth 0 it) backlinks)))

(defun org-roam-widget-show-backlinks-p ()
  (org-roam--org-roam-file-p (buffer-file-name org-roam-buffer--current)))

(defun org-roam-widget-render-backlinks (items)
  (let ((empty t)
        key values prop)
    (dolist (item items)
      (setq key (car item)
            values (cdr item)
            empty nil)
      (magit-insert-section (backlinks-file)
        (propertize (org-roam-db--get-title key)
                    'font-lock-face 'org-roam-title)
        (magit-insert-heading (concat
                               (org-fontify-like-in-org-mode
                                (org-roam-format-link key (org-roam-db--get-title key)))
                               ":"))
        (dolist (value values)
          (setq prop (nth 2 value))
          (let ((outline (or (plist-get prop :outline) '("Top")))
                (content (or (plist-get prop :content) "")))
            (magit-insert-section (backlink-outline)
              (magit-insert-heading (org-fontify-like-in-org-mode
                                     (-> outline
                                         (string-join " > ")
                                         (org-roam-buffer-expand-links key))))
              (magit-insert-section (backlink-preview)
                (insert (org-fontify-like-in-org-mode content) "\n")))))))
    empty))

(defvar org-roam-widget-backlinks
  (org-roam-widget :name 'backlinks
                   :header "Backlinks:"
                   :items #'org-roam-widget-get-backlinks
                   :render #'org-roam-widget-render-backlinks
                   :show-p #'org-roam-widget-show-backlinks-p)
  "Widget for backlinks.")

;;;; Citelinks Widget
(defun org-roam-widget-show-reflinks-p ()
  (org-roam--org-roam-file-p (buffer-file-name org-roam-buffer--current)))

(defun org-roam-widget-get-reflinks ()
  (when-let* ((refs (with-temp-buffer
                      (insert-buffer-substring org-roam-buffer--current)
                      (org-roam--extract-refs)))
              (paths (mapcar #'cdr refs))
              (key-backlinks (mapcan #'org-roam--get-backlinks paths)))
    (--group-by (nth 0 it) key-backlinks)))

(defun org-roam-widget-render-reflinks (items)
  (let ((empty t)
        file-from reflinks content)
    (dolist (item items)
      (setq file-from (car item)
            reflinks (cdr item)
            empty nil)
      (magit-insert-section (reflinks-file)
        (magit-insert-heading
          (org-fontify-like-in-org-mode
           (org-roam-format-link file-from
                                 (org-roam-db--get-title file-from)
                                 "file")))
        (dolist (reflink reflinks)
          (pcase-let ((`(_ _ ,props) reflink))
            (setq content (or (plist-get props :content) ""))
            (magit-insert-section (reflink)
              (insert (org-fontify-like-in-org-mode content) "\n"))))))
    empty))

(defvar org-roam-widget-reflinks
  (org-roam-widget :name 'reflinks
                   :header "Reflinks:"
                   :items #'org-roam-widget-get-reflinks
                   :render #'org-roam-widget-render-reflinks
                   :show-p #'org-roam-widget-show-reflinks-p)
  "Widget for reflinks.")

;;;; Unlinked References Widget
(defface org-roam-rowcol
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the rowcol part of the widgets."
  :group 'org-roam-faces)

(defvar org-roam-unlinked-reference-result-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

(defun org-roam-widget-show-unlinked-references-p ()
  (and (executable-find "rg")
       (not (string-match "PCRE2 is not available" (shell-command-to-string "rg --pcre2-version")))
       (org-roam--org-roam-file-p (buffer-file-name org-roam-buffer--current))))

(defun org-roam-widget-get-unlinked-references ()
  (let* ((titles (with-temp-buffer
                   (insert-buffer-substring org-roam-buffer--current)
                   (org-roam--extract-titles)))
         (rg-command (concat "rg -o --vimgrep -P -i "
                             (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                  (org-roam--list-files-search-globs org-roam-file-extensions)) " ")
                             (format " '\\[([^[]]++|(?R))*\\]%s' "
                                     (mapconcat (lambda (title)
                                                  (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                titles ""))
                             org-roam-directory)))
    (split-string (shell-command-to-string rg-command) "\n")))

(defun org-roam-widget-render-unlinked-references (lines)
  (let ((titles (with-temp-buffer
                   (insert-buffer-substring org-roam-buffer--current)
                   (org-roam--extract-titles)))
        (empty t)
        file row col match)
    (dolist (line lines)
      (save-match-data
        (when (string-match org-roam-unlinked-reference-result-re line)
          (setq file (match-string 1 line)
                row (match-string 2 line)
                col (match-string 3 line)
                match (match-string 4 line))
          (when (and match
                     (member (downcase match) (mapcar #'downcase titles))
                     (not (f-equal-p (expand-file-name file org-roam-directory)
                                     (buffer-file-name org-roam-buffer--current))))
            (magit-insert-section (unlinked-reference)
              (insert (propertize (format
                                   "%-8s"
                                   (format "%s:%s" row col))
                                  'font-lock-face 'org-roam-rowcol)
                      " "
                      (org-roam-db--get-title file)
                      "\n"))
            (setq empty nil)))))
    empty))

(defvar org-roam-widget-unlinked-references
  (org-roam-widget :name 'unlinked-refs
                   :header "Unlinked References:"
                   :items #'org-roam-widget-get-unlinked-references
                   :render #'org-roam-widget-render-unlinked-references
                   :show-p #'org-roam-widget-show-unlinked-references-p)
  "Widget for unlinked references.")

;;;
(defconst org-roam-widgets
  (list org-roam-widget-backlinks
        org-roam-widget-reflinks
        org-roam-widget-unlinked-references)
  "List of Org-roam widgets.")

;; Current Test Function
(defun org-roam-widget ()
  (interactive)
  (let ((buffer (get-buffer-create "*org-roam*")))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (magit-section-mode)
         (magit-insert-section (demo-buffer)
           (dolist (widget org-roam-widgets)
             (when (org-roam-widget-show widget)
               (org-roam-widget-render widget))))))
     (switch-to-buffer-other-window buffer)))
