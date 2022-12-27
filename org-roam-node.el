;;; org-roam-node.el --- Interfacing and interacting with nodes -*- lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.4") (magit-section "3.0.0"))

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
;; This module is dedicated for Org-roam nodes and its components. It provides
;; standard means to interface with them, both programmatically and
;; interactively.
;;
;;; Code:
(require 'org-roam)

;;; Options
;;;; Completing-read
(defcustom org-roam-node-display-template "${title}"
  "Configures display formatting for Org-roam node.
Patterns of form \"${field-name:length}\" are interpolated based
on the current node.

Each \"field-name\" is replaced with the return value of each
corresponding accessor function for `org-roam-node', e.g.
\"${title}\" will be interpolated by the result of
`org-roam-node-title'. You can also define custom accessors using
`cl-defmethod'. For example, you can define:

  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (concat \"My \" (org-roam-node-title node)))

and then reference it here or in the capture templates as
\"${my-title}\".

\"length\" is an optional specifier and declares how many
characters can be used to display the value of the corresponding
field. If it's not specified, the field will be inserted as is,
i.e. it won't be aligned nor trimmed. If it's an integer, the
field will be aligned accordingly and all the exceeding
characters will be trimmed out. If it's \"*\", the field will use
as many characters as possible and will be aligned accordingly.

A closure can also be assigned to this variable in which case the
closure is evaluated and the return value is used as the
template. The closure must evaluate to a valid template string."
  :group 'org-roam
  :type  '(string function))

(defcustom org-roam-node-annotation-function #'org-roam-node-read--annotation
  "This function used to attach annotations for `org-roam-node-read'.
It takes a single argument NODE, which is an `org-roam-node' construct."
  :group 'org-roam
  :type 'function)

(defcustom org-roam-node-default-sort 'file-mtime
  "Default sort order for Org-roam node completions."
  :type '(choice
          (const :tag "none" nil)
          (const :tag "file-mtime" file-mtime)
          (const :tag "file-atime" file-atime))
  :group 'org-roam)

(defcustom org-roam-node-formatter nil
  "The link description for node insertion.
If a function is provided, the function should take a single
argument, an `org-roam-node', and return a string.

If a string is provided, it is a template string expanded by
`org-roam-node--format-entry'."
  :group 'org-roam
  :type '(string function))

(defcustom org-roam-node-template-prefixes
  '(("tags" . "#")
    ("todo" . "t:"))
  "Prefixes for each of the node's properties.
This is used in conjunction with
`org-roam-node-display-template': in minibuffer completions the
node properties will be prefixed with strings in this variable,
acting as a query language of sorts.

For example, if a node has tags (\"foo\" \"bar\") and the alist
has the entry (\"tags\" . \"#\"), these will appear as
\"#foo #bar\"."
  :group 'org-roam
  :type  '(alist))

(defcustom org-roam-ref-annotation-function #'org-roam-ref-read--annotation
  "This function used to attach annotations for `org-roam-ref-read'.
It takes a single argument REF, which is a propertized string."
  :group 'org-roam
  :type  '(function))

;;;; Completion-at-point
(defcustom org-roam-completion-everywhere nil
  "When non-nil, provide link completion matching outside of Org links."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-completion-functions (list #'org-roam-complete-link-at-point
                                               #'org-roam-complete-everywhere)
  "List of functions to be used with `completion-at-point' for Org-roam."
  :group 'org-roam
  :type 'hook)

;;;; Linkage
(defcustom org-roam-link-auto-replace t
  "If non-nil, replace \"roam:\" links to existing nodes with \"id:\" links."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-extract-new-file-path "%<%Y%m%d%H%M%S>-${slug}.org"
  "The file path template to use when a node is extracted to its own file.
This path is relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defvar org-roam-node-history nil
  "Minibuffer history of nodes.")

(defvar org-roam-ref-history nil
  "Minibuffer history of refs.")

;;; Definition
(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
                             (:copier nil))
  "A heading or top level file with an assigned ID property."
  file file-title file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs)

;; Shim `string-glyph-compose' and `string-glyph-decompose' for Emacs versions that do not have it.
;; The functions were introduced in emacs commit 3f096eb3405b2fce7c35366eb2dcf025dda55783 and the
;; (original) functions behind them aren't autoloaded anymore.
(dolist (sym.replace
         '((string-glyph-compose . ucs-normalize-NFC-string)
           (string-glyph-decompose . ucs-normalize-NFD-string)))
  (let ((emacs-29-symbol (car sym.replace))
        (previous-implementation (cdr sym.replace)))
    (unless (fboundp emacs-29-symbol)
      (defalias emacs-29-symbol previous-implementation))))

(cl-defmethod org-roam-node-slug ((node org-roam-node))
  "Return the slug of NODE."
  (let ((title (org-roam-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                      ("__*" . "_")                   ;; remove sequential underscores
                      ("^_" . "")                     ;; remove starting underscore
                      ("_$" . "")))                   ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(cl-defmethod org-roam-node-formatted ((node org-roam-node))
  "Return a formatted string for NODE."
  (pcase org-roam-node-formatter
    ((pred functionp)
     (funcall org-roam-node-formatter node))
    ((pred stringp)
     (org-roam-node--format-entry (org-roam-node--process-display-format org-roam-node-formatter) node))
    (_
     (org-roam-node-title node))))

(cl-defmethod org-roam-node-category ((node org-roam-node))
  "Return the category for NODE."
  (cdr (assoc-string "CATEGORY" (org-roam-node-properties node))))

;;; Nodes
;;;; Getters
(defun org-roam-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error if there is no node at point.
This function also returns the node if it has yet to be cached in the
database. In this scenario, only expect `:id' and `:point' to be
populated."
  (or (magit-section-case
        (org-roam-node-section (oref it node))
        (org-roam-preview-section (save-excursion
                                    (magit-section-up)
                                    (org-roam-node-at-point)))
        (t (org-with-wide-buffer
            (while (not (or (org-roam-db-node-p)
                            (bobp)
                            ;; Handle case where top-level is a heading
                            (= (funcall outline-level)
                               (save-excursion
                                 (org-roam-up-heading-or-point-min)
                                 (funcall outline-level)))))
              (org-roam-up-heading-or-point-min))
            (when-let ((id (org-id-get)))
              (org-roam-populate
               (org-roam-node-create
                :id id
                :point (point)))))))
      (and assert (user-error "No node at point"))))

(defun org-roam-node-from-id (id)
  "Return an `org-roam-node' for the node containing ID.
Return nil if a node with ID does not exist."
  (when (> (caar (org-roam-db-query [:select (funcall count) :from nodes
                                     :where (= id $s1)]
                                    id)) 0)
    (org-roam-populate (org-roam-node-create :id id))))

(defun org-roam-node-from-title-or-alias (s)
  "Return an `org-roam-node' for the node with title or alias S.
Return nil if the node does not exist.
Throw an error if multiple choices exist."
  (let ((matches (seq-uniq
                  (append
                   (org-roam-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                      s)
                   (org-roam-db-query [:select [node-id] :from aliases
                                       :where (= alias $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(defun org-roam-node-from-ref (ref)
  "Return an `org-roam-node' from REF reference.
Return nil if there's no node with such REF."
  (save-match-data
    (let (type path)
      (cond
       ((string-match org-link-plain-re ref)
        (setq type (match-string 1 ref)
              path (match-string 2 ref)))
       ((string-prefix-p "@" ref)
        (setq type "cite"
              path (substring ref 1))))
      (when (and type path)
        (when-let ((id (caar (org-roam-db-query
                              [:select [nodes:id]
                               :from refs
                               :left-join nodes
                               :on (= refs:node-id nodes:id)
                               :where (= refs:type $s1)
                               :and (= refs:ref $s2)
                               :limit 1]
                              type path))))
          (org-roam-populate (org-roam-node-create :id id)))))))

(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (when-let ((node-info (car (org-roam-db-query [:select [file level pos todo priority
                                                          scheduled deadline title properties olp]
                                                 :from nodes
                                                 :where (= id $s1)
                                                 :limit 1]
                                                (org-roam-node-id node)))))
    (pcase-let* ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title ,properties ,olp) node-info)
                 (`(,atime ,mtime ,file-title) (car (org-roam-db-query [:select [atime mtime title]
                                                                        :from files
                                                                        :where (= file $s1)]
                                                                       file)))
                 (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                             :where (= node-id $s1)]
                                                            (org-roam-node-id node))))
                 (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                               :where (= node-id $s1)]
                                                              (org-roam-node-id node))))
                 (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                              :where (= node-id $s1)]
                                                             (org-roam-node-id node)))))
      (setf (org-roam-node-file node) file
            (org-roam-node-file-title node) file-title
            (org-roam-node-file-atime node) atime
            (org-roam-node-file-mtime node) mtime
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-properties node) properties
            (org-roam-node-olp node) olp
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info)))
  node)

(defun org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

;;;; Finders
(defun org-roam-node-marker (node)
  "Get the marker for NODE."
  (unwind-protect
      (let* ((file (org-roam-node-file node))
             (buffer (or (find-buffer-visiting file)
                         (find-file-noselect file))))
        (with-current-buffer buffer
          (move-marker (make-marker) (org-roam-node-point node) buffer)))))

(defun org-roam-node-open (node &optional cmd force)
  "Go to the node NODE.
CMD is the command used to display the buffer. If not provided,
`org-link-frame-setup' is respected. Assumes that the node is
fully populated, with file and point. If NODE is already visited,
this won't automatically move the point to the beginning of the
NODE, unless FORCE is non-nil."
  (interactive (list (org-roam-node-at-point) current-prefix-arg))
  (org-mark-ring-push)
  (let ((m (org-roam-node-marker node))
        (cmd (or cmd
                 (cdr
                  (assq
                   (cdr (assq 'file org-link-frame-setup))
                   '((find-file . switch-to-buffer)
                     (find-file-other-window . switch-to-buffer-other-window)
                     (find-file-other-frame . switch-to-buffer-other-frame))))
                 'switch-to-buffer-other-window)))
    (if (not (equal (current-buffer) (marker-buffer m)))
        (funcall cmd (marker-buffer m)))
    (when (or force
              (not (equal (org-roam-node-id node)
                          (org-roam-id-at-point))))
      (goto-char m))
    (move-marker m nil))
  (org-show-context))

(defun org-roam-node-visit (node &optional other-window force)
  "From the current buffer, visit NODE. Return the visited buffer.
Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

If NODE is already visited, this won't automatically move the
point to the beginning of the NODE, unless FORCE is non-nil. In
interactive calls FORCE always set to t."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg t))
  (org-roam-node-open node (if other-window
                               #'switch-to-buffer-other-window
                             #'pop-to-buffer-same-window)
                      force))

;;;###autoload
(cl-defun org-roam-node-find (&optional other-window initial-input filter-fn pred &key templates)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)"
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn pred)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :templates templates
       :props '(:finalize find-file)))))

;;;###autoload
(defun org-roam-node-random (&optional other-window filter-fn)
  "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive current-prefix-arg)
  (org-roam-node-visit
   (cdr (seq-random-elt (org-roam-node-read--completions filter-fn)))
   other-window))

;;;; Completing-read interface
(defun org-roam-node-read (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match.
PROMPT is a string to show at the beginning of the mini-buffer, defaulting to \"Node: \""
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn))
         (prompt (or prompt "Node: "))
         (node (completing-read
                prompt
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        ;; Preserve sorting in the completion UI if a sort-fn is used
                        ,@(when sort-fn
                            '((display-sort-function . identity)
                              (cycle-sort-function . identity)))
                        (annotation-function
                         . ,(lambda (title)
                              (funcall org-roam-node-annotation-function
                                       (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (complete-with-action action nodes string pred)))
                nil require-match initial-input 'org-roam-node-history)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

(defun org-roam-node-read--completions (&optional filter-fn sort-fn)
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
The displayed title is formatted according to `org-roam-node-display-template'."
  (let* ((template (org-roam-node--process-display-format org-roam-node-display-template))
         (nodes (org-roam-node-list))
         (nodes (if filter-fn
                    (cl-remove-if-not
                     (lambda (n) (funcall filter-fn n))
                     nodes)
                  nodes))
         (nodes (mapcar (lambda (node)
                          (org-roam-node-read--to-candidate node template)) nodes))
         (sort-fn (or sort-fn
                      (when org-roam-node-default-sort
                        (intern (concat "org-roam-node-read-sort-by-"
                                        (symbol-name org-roam-node-default-sort))))))
         (nodes (if sort-fn (seq-sort sort-fn nodes)
                  nodes)))
    nodes))

(defun org-roam-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
                         template
                         node
                         (1- (if (bufferp (current-buffer))
                                 (window-width) (frame-width))))))
    (cons (propertize candidate-main 'node node) node)))

(defun org-roam-node--format-entry (template node &optional width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
TEMPLATE is the processed template used to format the entry."
  (pcase-let ((`(,tmpl . ,tmpl-width) template))
    (org-roam-format-template
     tmpl
     (lambda (field _default-val)
       (pcase-let* ((`(,field-name ,field-width) (split-string field ":"))
                    (getter (intern (concat "org-roam-node-" field-name)))
                    (field-value (funcall getter node)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (file-relative-name field-value org-roam-directory)))
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (when (and field-value (not (listp field-value)))
           (setq field-value (list field-value)))
         (setq field-value (mapconcat
                            (lambda (v)
                              (concat (or (cdr (assoc field-name org-roam-node-template-prefixes))
                                          "")
                                      v))
                            field-value " "))
         (setq field-width (cond
                            ((not field-width)
                             field-width)
                            ((string-equal field-width "*")
                             (if width
                                 (- width tmpl-width)
                               tmpl-width))
                            ((>= (string-to-number field-width) 0)
                             (string-to-number field-width))))
         (when field-width
           (let* ((truncated (truncate-string-to-width field-value field-width 0 ?\s))
                  (tlen (length truncated))
                  (len (length field-value)))
             (if (< tlen len)
                 ;; Make the truncated part of the string invisible. If strings
                 ;; are pre-propertized with display or invisible properties, the
                 ;; formatting may get messed up. Ideally, truncated strings are
                 ;; not preformatted with these properties. Face properties are
                 ;; allowed without restriction.
                 (put-text-property tlen len 'invisible t field-value)
               ;; If the string wasn't truncated, but padded, use this string instead.
               (setq field-value truncated))))
         field-value)))))

(defun org-roam-node--process-display-format (format)
  "Pre-calculate minimal widths needed by the FORMAT string."
  (let* ((fields-width 0)
         (string-width
          (string-width
           (org-roam-format-template
            format
            (lambda (field _default-val)
              (setq fields-width
                    (+ fields-width
                       (string-to-number
                        (or (cadr (split-string field ":"))
                            "")))))))))
    (cons format (+ fields-width string-width))))

(defun org-roam-node-read-sort-by-file-mtime (completion-a completion-b)
  "Sort files such that files modified more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of
\(node-title org-roam-node-struct)"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-mtime node-b)
                 (org-roam-node-file-mtime node-a))))

(defun org-roam-node-read-sort-by-file-atime (completion-a completion-b)
  "Sort files such that files accessed more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of
\(node-title org-roam-node-struct)"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-node-file-atime node-b)
                 (org-roam-node-file-atime node-a))))

(defun org-roam-node-read--annotation (_node)
  "Placeholder function. Return empty string for annotations."
  "")

;;;; Linkage
;;;;; [id:] link
;;;###autoload
(cl-defun org-roam-node-insert (&optional filter-fn &key templates info)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-formatted node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (let ((id (org-roam-node-id node)))
                  (insert (org-link-make-string
                           (concat "id:" id)
                           description))
                  (run-hook-with-args 'org-roam-post-node-insert-hook
                                      id
                                      description)))
            (org-roam-capture-
             :node node
             :info info
             :templates templates
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

;;;;; [roam:] link
(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link)
(defun org-roam-link-follow-link (title-or-alias)
  "Navigate \"roam:\" link to find and open the node with TITLE-OR-ALIAS.
Assumes that the cursor was put where the link is."
  (if-let ((node (org-roam-node-from-title-or-alias title-or-alias)))
      (progn
        (when org-roam-link-auto-replace
          (org-roam-link-replace-at-point))
        (org-mark-ring-push)
        (org-roam-node-visit node nil 'force))
    (org-roam-capture-
     :node (org-roam-node-create :title title-or-alias)
     :props '(:finalize find-file))))

(defun org-roam-link-replace-at-point (&optional link)
  "Replace \"roam:\" LINK at point with an \"id:\" link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link))))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   (setq node (save-match-data (org-roam-node-from-title-or-alias path))))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-node-id node))
                          (or desc path))))))))

(defun org-roam-link-replace-all ()
  "Replace all \"roam:\" links in buffer with \"id:\" links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (org-roam-link-replace-at-point))))

(add-hook 'org-roam-find-file-hook #'org-roam--replace-roam-links-on-save-h)
(defun org-roam--replace-roam-links-on-save-h ()
  "Run `org-roam-link-replace-all' before buffer is saved to its file."
  (when org-roam-link-auto-replace
    (add-hook 'before-save-hook #'org-roam-link-replace-all nil t)))

;;;;;; Completion-at-point interface
(defconst org-roam-bracket-completion-re
  "\\[\\[\\(\\(?:roam:\\)?\\)\\([^z-a]*?\\)]]"
  "Regex for completion within link brackets.
We use this as a substitute for `org-link-bracket-re', because
`org-link-bracket-re' requires content within the brackets for a match.")

(defun org-roam-complete-link-at-point ()
  "Complete \"roam:\" link at point to an existing Org-roam node."
  (let (roam-p start end)
    (when (org-in-regexp org-roam-bracket-completion-re 1)
      (setq roam-p (not (or (org-in-src-block-p)
                            (string-blank-p (match-string 1))))
            start (match-beginning 2)
            end (match-end 2))
      (list start end
            (org-roam--get-titles)
            :exit-function
            (lambda (str &rest _)
              (delete-char (- 0 (length str)))
              (insert (concat (unless roam-p "roam:")
                              str))
              (forward-char 2))))))

(defun org-roam-complete-everywhere ()
  "Complete symbol at point as a link completion to an Org-roam node.
This is a `completion-at-point' function, and is active when
`org-roam-completion-everywhere' is non-nil.

Unlike `org-roam-complete-link-at-point' this will complete even
outside of the bracket syntax for links (i.e. \"[[roam:|]]\"),
hence \"everywhere\"."
  (when (and org-roam-completion-everywhere
             (thing-at-point 'word)
             (not (org-in-src-block-p))
             (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            (org-roam--get-titles)
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert "[[roam:" str "]]"))
            ;; Proceed with the next completion function if the returned titles
            ;; do not match. This allows the default Org capfs or custom capfs
            ;; of lower priority to run.
            :exclusive 'no))))

(add-hook 'org-roam-find-file-hook #'org-roam--register-completion-functions-h)
(add-hook 'org-roam-indirect-buffer-hook #'org-roam--register-completion-functions-h)

(defun org-roam--register-completion-functions-h ()
  "Setup `org-roam-completion-functions' for `completion-at-point'."
  (dolist (f org-roam-completion-functions)
    (add-hook 'completion-at-point-functions f nil t)))

;;;; Editing
(defun org-roam-demote-entire-buffer ()
  "Convert an org buffer with any top level content to a single node.

All headings are demoted one level.

The #+TITLE: keyword is converted into a level-1 heading and deleted.
Any tags declared on #+FILETAGS: are transferred to tags on the new top heading.

Any top level properties drawers are incorporated into the new heading."
  (interactive)
  (org-with-point-at 1
    (org-map-region #'org-do-demote
                    (point-min) (point-max))
    (insert "* "
            (org-roam--get-keyword "title")
            "\n")
    (org-back-to-heading)
    (org-set-tags (org-roam--get-keyword "filetags"))
    (org-roam-erase-keyword "title")
    (org-roam-erase-keyword "filetags")))

(defun org-roam--h1-count ()
  "Count level-1 headings in the current file."
  (let ((h1-count 0))
    (org-with-wide-buffer
     (org-map-region (lambda ()
                       (if (= (org-current-level) 1)
                           (cl-incf h1-count)))
                     (point-min) (point-max))
     h1-count)))

(defun org-roam--buffer-promoteable-p ()
  "Verify that this buffer is promoteable:
There is a single level-1 heading
and no extra content before the first heading."
  (and
   (= (org-roam--h1-count) 1)
   (org-with-point-at 1 (org-at-heading-p))))

(defun org-roam-promote-entire-buffer ()
  "Promote the current buffer.
Converts a file containing a single level-1 headline node to a file
node."
  (interactive)
  (unless (org-roam--buffer-promoteable-p)
    (user-error "Cannot promote: multiple root headings or there is extra file-level text"))
  (org-with-point-at 1
    (let ((title (nth 4 (org-heading-components)))
          (tags (org-get-tags)))
      (kill-whole-line)
      (org-roam-end-of-meta-data t)
      (insert "#+title: " title "\n")
      (when tags (org-roam-tag-add tags))
      (org-map-region #'org-promote (point-min) (point-max))
      (org-roam-db-update-file))))

;;;###autoload
(defun org-roam-refile ()
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if (equal (org-roam-node-at-point) node)
        (user-error "Target is the same as current node")
      (if regionp
          (progn
            (org-kill-new (buffer-substring region-start region-end))
            (org-save-markers-in-region region-start region-end))
        (progn
          (if (org-before-first-heading-p)
              (org-roam-demote-entire-buffer))
          (org-copy-subtree 1 nil t)))
      (with-current-buffer nbuf
        (org-with-wide-buffer
         (goto-char (org-roam-node-point node))
         (setq level (org-get-valid-level (funcall outline-level) 1)
               reversed (org-notes-order-reversed-p))
         (goto-char
          (if reversed
              (or (outline-next-heading) (point-max))
            (or (save-excursion (org-get-next-sibling))
                (org-end-of-subtree t t)
                (point-max))))
         (unless (bolp) (newline))
         (org-paste-subtree level nil nil t)
         (and org-auto-align-tags
              (let ((org-loop-over-headlines-in-active-region nil))
                (org-align-tags)))
         (when (fboundp 'deactivate-mark) (deactivate-mark))))
      (if regionp
          (delete-region (point) (+ (point) (- region-end region-start)))
        (org-preserve-local-variables
         (delete-region
          (and (org-back-to-heading t) (point))
          (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
      ;; If the buffer end-up empty after the refile, kill it and delete its
      ;; associated file.
      (when (eq (buffer-size) 0)
        (if (buffer-file-name)
            (delete-file (buffer-file-name)))
        (set-buffer-modified-p nil)
        ;; If this was done during capture, abort the capture process.
        (when (and org-capture-mode
                   (buffer-base-buffer (current-buffer)))
          (org-capture-kill))
        (kill-buffer (current-buffer))))))

;;;###autoload
(defun org-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (let* ((template-info nil)
           (node (org-roam-node-at-point))
           (template (org-roam-format-template
                      (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                      (lambda (key default-val)
                        (let ((fn (intern key))
                              (node-fn (intern (concat "org-roam-node-" key)))
                              (ksym (intern (concat ":" key))))
                          (cond
                           ((fboundp fn)
                            (funcall fn node))
                           ((fboundp node-fn)
                            (funcall node-fn node))
                           (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                (plist-put template-info ksym r)
                                r)))))))
           (file-path
            (expand-file-name
             (read-file-name "Extract node to: "
                             (file-name-as-directory org-roam-directory) template nil template)
             org-roam-directory)))
      (when (file-exists-p file-path)
        (user-error "%s exists. Aborting" file-path))
      (org-cut-subtree)
      (save-buffer)
      (with-current-buffer (find-file-noselect file-path)
        (org-paste-subtree)
        (while (> (org-current-level) 1) (org-promote-subtree))
        (save-buffer)
        (org-roam-promote-entire-buffer)
        (save-buffer)))))

;;; Refs
;;;; Completing-read interface
(defun org-roam-ref-read (&optional initial-input filter-fn)
  "Read an Org-roam ref and return a corresponding `org-roam-node'.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
filtered out."
  (let* ((refs (org-roam-ref-read--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (completing-read "Ref: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     `(metadata
                                       (annotation-function
                                        . ,org-roam-ref-annotation-function)
                                       (category . org-roam-ref))
                                   (complete-with-action action refs string pred)))
                               nil t initial-input 'org-roam-ref-history)))
    (cdr (assoc ref refs))))

(defun org-roam-ref-read--completions ()
  "Return an alist for ref completion.
The car is the ref, and the cdr is the corresponding node for the ref."
  (let ((rows (org-roam-db-query
               [:select [id ref type nodes:file pos title]
                :from refs
                :left-join nodes
                :on (= refs:node-id nodes:id)])))
    (cl-loop for row in rows
             collect (pcase-let* ((`(,id ,ref ,type ,file ,pos ,title) row)
                                  (node (org-roam-node-create :id id
                                                              :file file
                                                              :point pos
                                                              :title title)))
                       (cons
                        (concat (propertize ref 'node node 'type type)
                                (propertize id 'invisible t))
                        node)))))

(defun org-roam-ref-read--annotation (ref)
  "Return the annotation for REF, which assumed to be a propertized string."
  (let* ((node (get-text-property 0 'node ref))
         (title (org-roam-node-title node)))
    (when title
      (concat " " title))))

;;;; Finders
;;;###autoload
(defun org-roam-ref-find (&optional initial-input filter-fn)
  "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (let* ((node (org-roam-ref-read initial-input filter-fn)))
    (org-roam-node-visit node)))

;;;; Editing
(defun org-roam-ref-add (ref)
  "Add REF to the node at point."
  (interactive "sRef: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-property-add "ROAM_REFS" (if (memq " " (string-to-list ref))
                                             (concat "\"" ref "\"")
                                           ref)))))

(defun org-roam-ref-remove (&optional ref)
  "Remove a REF from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-property-remove "ROAM_REFS" ref))))

;;; Tags
;;;; Getters
(defun org-roam-tag-completions ()
  "Return list of tags for completions within Org-roam."
  (let ((roam-tags (mapcar #'car (org-roam-db-query [:select :distinct [tag] :from tags])))
        (org-tags (cl-loop for tagg in org-tag-alist
                           nconc (pcase tagg
                                   ('(:newline)
                                    nil)
                                   (`(,tag . ,_)
                                    (list tag))
                                   (_ nil)))))
    (seq-uniq (append roam-tags org-tags))))

;;;; Editing
(defun org-roam-tag-add (tags)
  "Add TAGS to the node at point."
  (interactive
   (list (let ((crm-separator "[ 	]*:[ 	]*"))
           (completing-read-multiple "Tag: " (org-roam-tag-completions)))))
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                "")
                                            ":" 'omit-nulls)))
            (org-roam-set-keyword "filetags" (org-make-tag-string (seq-uniq (append tags current-tags)))))
        (org-set-tags (seq-uniq (append tags (org-get-tags)))))
      tags)))

(defun org-roam-tag-remove (&optional tags)
  "Remove TAGS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let* ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                              (org-collect-keywords '("filetags"))))
                                                 (user-error "No tag to remove"))
                                             ":" 'omit-nulls))
                 (tags (or tags (completing-read-multiple "Tag: " current-tags))))
            (org-roam-set-keyword "filetags"
                                  (org-make-tag-string (seq-difference current-tags tags #'string-equal))))
        (let* ((current-tags (or (org-get-tags)
                                 (user-error "No tag to remove")))
               (tags (or tags (completing-read-multiple "Tag: " current-tags))))
          (org-set-tags (seq-difference current-tags tags #'string-equal))))
      tags)))

;;; Titles and Aliases
;;;; Getters
(defun org-roam--get-titles ()
  "Return all distinct titles and aliases in the Org-roam database."
  (mapcar #'car (org-roam-db-query [:select :distinct title :from nodes
                                    :union :select alias :from aliases])))

;;;; Editing
(defun org-roam-alias-add (alias)
  "Add ALIAS to the node at point."
  (interactive "sAlias: ")
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-property-add "ROAM_ALIASES" alias))))

(defun org-roam-alias-remove (&optional alias)
  "Remove an ALIAS from the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (org-roam-property-remove "ROAM_ALIASES" alias))))


(provide 'org-roam-node)
;;; org-roam-node.el ends here
