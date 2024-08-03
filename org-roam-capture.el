;;; org-roam-capture.el --- Capture functionality -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.4") (emacsql "20230228") (magit-section "3.0.0"))

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
;; This module provides `org-capture' functionality for Org-roam. With this
;; module the user can capture new nodes or capture new content to existing
;; nodes.
;;
;;; Code:
(require 'org-roam)

;;;; Declarations
(defvar org-end-time-was-given)

;;; Options
(defcustom org-roam-capture-templates
  '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t))
  "Templates for the creation of new entries within Org-roam.

Each entry is a list with the following items:

keys   The keys that will select the template, as a string, characters only, for
       example \"a\" for a template to be selected with a single key, or
       \"bt\" for selection with two keys. When using several keys, keys
       using the same prefix must be together in the list and preceded by a
       2-element entry explaining the prefix key, for example:

                   (\"b\" \"Templates for marking stuff to buy\")

       The \"C\" key is used by default for quick access to the customization of
       the template variable. But if you want to use that key for a template,
       you can.

description   A short string describing the template, which will be shown
              during selection.

type       The type of entry. Valid types are:
               entry       an Org node, with a headline.  Will be filed
                           as the child of the target entry or as a
                           top level entry.  Its default template is:
                             \"* %?\n %a\"
               item        a plain list item, will be placed in the
                           first plain list at the target location.
                           Its default template is:
                             \"- %?\"
               checkitem   a checkbox item.  This differs from the
                           plain list item only in so far as it uses a
                           different default template.  Its default
                           template is:
                             \"- [ ] %?\"
               table-line  a new line in the first table at target location.
                           Its default template is:
                             \"| %? |\"
               plain       text to be inserted as it is.

template     The template for creating the capture item.
             If it is an empty string or nil, a default template based on
             the entry type will be used (see the \"type\" section above).
             Instead of a string, this may also be one of:

                 (file \"/path/to/template-file\")
                 (function function-returning-the-template)

             in order to get a template from a file, or dynamically
             from a function.

The template contains a compulsory :target property. The :target property
contains a list, where:
  - The first element indicates the type of the target.
  - The second element indicates the location of the captured node.
  - And the rest of the list indicate the prefilled template, that will be
    inserted and the position of the point will be adjusted for.
    This behavior varies from type to type.

The following options are supported for the :target property:

   (file \"path/to/file\")
       The file will be created, and prescribed an ID.

   (file+head \"path/to/file\" \"head content\")
       The file will be created, prescribed an ID, and head content will be
       inserted if the node is a newly captured one.

   (file+olp \"path/to/file\" (\"h1\" \"h2\"))
       The file will be created, prescribed an ID. If the file doesn't contain
       the outline path (h1, h2), it will be automatically created. The point
       will be adjusted to the last element in the OLP.

   (file+head+olp \"path/to/file\" \"head content\" (\"h1\" \"h2\"))
       The file will be created, prescribed an ID. Head content will be
       inserted at the start of the file if the node is a newly captured one.
       If the file doesn't contain the outline path (h1, h2), it will be
       automatically created. The point will be adjusted to the last element in
       the OLP.

   (file+datetree \"path/to/file\" tree-type)
       The file will be created, prescribed an ID. A date based outline path
       will be created for today's date. The tree-type can be one of the
       following symbols: day, week or month. The point will adjusted to the
       last element in the tree. To prompt for date instead of using today's,
       use the :time-prompt property.

   (node \"title or alias or ID of an existing node\")
       The point will be placed for an existing node, based on either, its
       title, alias or ID.

The rest of the entry is a property list of additional options.  Recognized
properties are:

 :prepend            Normally newly captured information will be appended at
                     the target location (last child, last table line,
                     last list item...).  Setting this property will
                     change that.

 :immediate-finish   When set, do not offer to edit the information, just
                     file it away immediately.  This makes sense if the
                     template only needs information that can be added
                     automatically.

 :jump-to-captured   When set, jump to the captured entry when finished.

 :empty-lines        Set this to the number of lines that should be inserted
                     before and after the new item.  Default 0, only common
                     other value is 1.

 :empty-lines-before Set this to the number of lines that should be inserted
                     before the new item.  Overrides :empty-lines for the
                     number lines inserted before.

 :empty-lines-after  Set this to the number of lines that should be inserted
                     after the new item.  Overrides :empty-lines for the
                     number of lines inserted after.

 :clock-in           Start the clock in this item.

 :clock-keep         Keep the clock running when filing the captured entry.

 :clock-resume       Start the interrupted clock when finishing the capture.
                     Note that :clock-keep has precedence over :clock-resume.
                     When setting both to t, the current clock will run and
                     the previous one will not be resumed.

 :time-prompt        Prompt for a date/time to be used for date/week trees
                     and when filling the template.

 :tree-type          When `week', make a week tree instead of the month-day
                     tree.  When `month', make a month tree instead of the
                     month-day tree.

 :unnarrowed         Do not narrow the target buffer, simply show the
                     full buffer.  Default is to narrow it so that you
                     only see the new stuff.

 :table-line-pos     Specification of the location in the table where the
                     new line should be inserted.  It should be a string like
                     \"II-3\", meaning that the new line should become the
                     third line before the second horizontal separator line.

 :kill-buffer        If the target file was not yet visited by a buffer when
                     capture was invoked, kill the buffer again after capture
                     is finalized.

 :no-save            Do not save the target file after finishing the capture.

The template defines the text to be inserted.  Often this is an
Org mode entry (so the first line should start with a star) that
will be filed as a child of the target headline.  It can also be
freely formatted text.  Furthermore, the following %-escapes will
be replaced with content and expanded:

  %[pathname] Insert the contents of the file given by
              `pathname'.  These placeholders are expanded at the very
              beginning of the process so they can be used to extend the
              current template.
  %(sexp)     Evaluate elisp `(sexp)' and replace it with the results.
              Only placeholders pre-existing within the template, or
              introduced with %[pathname] are expanded this way.  Since this
              happens after expanding non-interactive %-escapes, those can
              be used to fill the expression.
  %<...>      The result of `format-time-string' on the ... format
              specification.
  %t          Time stamp, date only.  The time stamp is the current time,
              except when called from agendas with `\\[org-agenda-capture]' or
              with `org-capture-use-agenda-date' set.
  %T          Time stamp as above, with date and time.
  %u, %U      Like the above, but inactive time stamps.
  %i          Initial content, copied from the active region.  If
              there is text before %i on the same line, such as
              indentation, and %i is not inside a %(sexp), that prefix
              will be added before every line in the inserted text.
  %a          Annotation, normally the link created with `org-store-link'.
  %A          Like %a, but prompt for the description part.
  %l          Like %a, but only insert the literal link.
  %L          Like %l, but without brackets (the link content itself).
  %c          Current kill ring head.
  %x          Content of the X clipboard.
  %k          Title of currently clocked task.
  %K          Link to currently clocked task.
  %n          User name (taken from the variable `user-full-name').
  %f          File visited by current buffer when `org-capture' was called.
  %F          Full path of the file or directory visited by current buffer.
  %:keyword   Specific information for certain link types, see below.
  %^g         Prompt for tags, with completion on tags in target file.
  %^G         Prompt for tags, with completion on all tags in all agenda files.
  %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
              You may define a prompt like: %^{Please specify birthday}t.
              The default date is that of %t, see above.
  %^C         Interactive selection of which kill or clip to use.
  %^L         Like %^C, but insert as link.
  %^{prop}p   Prompt the user for a value for property `prop'.
              A default value can be specified like this:
              %^{prop|default}p.
  %^{prompt}  Prompt the user for a string and replace this sequence with it.
              A default value and a completion table can be specified like this:
              %^{prompt|default|completion2|completion3|...}.
  %?          After completing the template, position cursor here.
  %\\1 ... %\\N Insert the text entered at the nth %^{prompt}, where N
              is a number, starting from 1.

Apart from these general escapes, you can access information specific to
the link type that is created.  For example, calling `org-capture' in emails
or in Gnus will record the author and the subject of the message, which you
can access with \"%:from\" and \"%:subject\", respectively.  Here is a
complete list of what is recorded for each link type.

Link type               |  Available information
------------------------+------------------------------------------------------
bbdb                    |  %:type %:name %:company
vm, wl, mh, mew, rmail, |  %:type %:subject %:message-id
gnus                    |  %:from %:fromname %:fromaddress
                        |  %:to   %:toname   %:toaddress
                        |  %:fromto (either \"to NAME\" or \"from NAME\")
                        |  %:date %:date-timestamp (as active timestamp)
                        |  %:date-timestamp-inactive (as inactive timestamp)
gnus                    |  %:group, for messages also all email fields
eww, w3, w3m            |  %:type %:url
info                    |  %:type %:file %:node
calendar                |  %:type %:date

When you need to insert a literal percent sign in the template,
you can escape ambiguous cases with a backward slash, e.g., \\%i.

In addition to all of the above, Org-roam supports additional
substitutions within its templates. \"${foo}\" will look for the
foo property in the Org-roam node (see the `org-roam-node'). If
the property does not exist, the user will be prompted to fill in
the string value.

Org-roam templates are NOT compatible with regular Org capture:
they rely on additional hacks and hooks to achieve the
streamlined user experience in Org-roam."
  :group 'org-roam
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :target)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

(defcustom org-roam-capture-new-node-hook nil
  "Normal-mode hooks run when a new Org-roam node is created.
The current point is the point of the new node.
The hooks must not move the point."
  :group 'org-roam
  :type 'hook)

(defvar org-roam-capture-preface-hook nil
  "Hook run when Org-roam tries to determine capture location of the node.
If any hook returns a value (which should be an ID), all hooks
after it are ignored.

With this hook you can hijack controls over the location of the
node for which the capture process is currently running for, or
use to just perform an arbitrary side effect, e.g. modify the
state related to the capture process. See `org-roam-protocol' and
`org-roam-dailies' as examples for what and how this hook is used
for.

If you're trying to perform the hijack, it's mandatory for you to:
  1. Set the currently active buffer for editing operations using
     `org-capture-target-buffer'.
  2. Place the point in this buffer from where the location starts
     from (e.g. if it's a file based node it should be the BOB,
     otherwise it should be the position from where the heading
     based node starts from).
  3. Return the ID (as a string) of the capturing node.

If you use this hook for any other purpose, but not the hijack,
it's mandatory that you should return nil as the return value; so
the capture process would be able to setup the capture buffer.

If you need to do something when you capture new nodes, use
`org-roam-capture-new-node-hook' instead of this hook.

WARNING: This hook is primarily designed for the usage by the
extensions and packages, and requires understanding of the
internal capture process. If you don't understand it, you should
learn these internals before using this or use it at your own
risk breaking things.")

;;; Variables

(defvar org-roam-capture--node nil
  "The node passed during an Org-roam capture.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar org-roam-capture--info nil
  "A property-list of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defconst org-roam-capture--template-keywords (list :target :id :link-description :call-location
                                                    :region)
  "Keywords used in `org-roam-capture-templates' specific to Org-roam.")

;;; Main entry point
;;;###autoload
(cl-defun org-roam-capture- (&key goto keys node info props templates)
  "Main entry point of `org-roam-capture' module.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is a plist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates."
  (let* ((props (plist-put props :call-location (point-marker)))
         (org-capture-templates
          (mapcar (lambda (template)
                    (org-roam-capture--convert-template template props))
                  (or templates org-roam-capture-templates)))
         (_ (setf (org-roam-node-id node) (or (org-roam-node-id node)
                                              (org-id-new))))
         (org-roam-capture--node node)
         (org-roam-capture--info info))
    (when (and (not keys)
               (= (length org-capture-templates) 1))
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)))

;;;###autoload
(cl-defun org-roam-capture (&optional goto keys &key filter-fn templates info)
  "Launches an `org-capture' process for a new or existing node.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed along to the underlying `org-roam-capture-'."
  (interactive "P")
  (let ((node (org-roam-node-read nil filter-fn)))
    (org-roam-capture- :goto goto
                       :info info
                       :keys keys
                       :templates templates
                       :node node
                       :props '(:immediate-finish nil))))

;;; Capture process
(defun org-roam-capture-p ()
  "Return t if the current capture process is an Org-roam capture.
This function is to only be called when `org-capture-plist' is
valid for the capture (i.e. initialization, and finalization of
the capture)."
  (plist-get org-capture-plist :org-roam))

(defun org-roam-capture--get (keyword)
  "Get the value for KEYWORD from the `org-roam-capture-template'."
  (plist-get (plist-get org-capture-plist :org-roam) keyword))

(defun org-roam-capture--put (prop value)
  "Set property PROP to VALUE in the `org-roam-capture-template'."
  (let ((p (plist-get org-capture-plist :org-roam)))
    (setq org-capture-plist
          (plist-put org-capture-plist
                     :org-roam
                     (plist-put p prop value)))))

;;;; Capture target
(defun org-roam-capture--prepare-buffer ()
  "Prepare the capture buffer for the current Org-roam based capture template.
This function will initialize and setup the capture buffer,
position the point to the current :target (and if necessary,
create it if it doesn't exist), and place the point for further
processing by `org-capture'.

Note: During the capture process this function is run by
`org-capture-set-target-location', as a (function ...) based
capture target."
  (let ((id (cond ((run-hook-with-args-until-success 'org-roam-capture-preface-hook))
                  (t (org-roam-capture--setup-target-location)))))
    (org-roam-capture--adjust-point-for-capture-type)
    (let ((template (org-capture-get :template)))
      (when (stringp template)
        (org-capture-put
         :template
         (org-roam-capture--fill-template template))))
    (org-roam-capture--put :id id)
    (org-roam-capture--put :finalize (or (org-capture-get :finalize)
                                         (org-roam-capture--get :finalize)))))

(defun org-roam-capture--setup-target-location ()
  "Initialize the buffer, and goto the location of the new capture.
Return the ID of the location."
  (let (p new-file-p)
    (pcase (org-roam-capture--get-target)
      (`(file ,path)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+olp ,path ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m))
       (widen))
      (`(file+head ,path ,head)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+head+olp ,path ,head ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m)))
      (`(file+datetree ,path ,tree-type)
       (setq path (org-roam-capture--target-truepath path))
       (require 'org-datetree)
       (widen)
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (funcall
        (pcase tree-type
          (`week #'org-datetree-find-iso-week-create)
          (`month #'org-datetree-find-month-create)
          (_ #'org-datetree-find-date-create))
        (calendar-gregorian-from-absolute
         (cond
          (org-overriding-default-time
           ;; Use the overriding default time.
           (time-to-days org-overriding-default-time))
          ((org-capture-get :default-time)
           (time-to-days (org-capture-get :default-time)))
          ((org-capture-get :time-prompt)
           ;; Prompt for date.  Bind `org-end-time-was-given' so
           ;; that `org-read-date-analyze' handles the time range
           ;; case and returns `prompt-time' with the start value.
           (let* ((org-time-was-given nil)
                  (org-end-time-was-given nil)
                  (prompt-time (org-read-date
                                nil t nil "Date for tree entry:")))
             (org-capture-put
              :default-time
              (if (or org-time-was-given
                      (= (time-to-days prompt-time) (org-today)))
                  prompt-time
                ;; Use 00:00 when no time is given for another
                ;; date than today?
                (apply #'encode-time 0 0
                       org-extend-today-until
                       (cl-cdddr (decode-time prompt-time)))))
             (time-to-days prompt-time)))
          (t
           ;; Current date, possibly corrected for late night
           ;; workers.
           (org-today)))))
       (setq p (point)))
      (`(node ,title-or-id)
       ;; first try to get ID, then try to get title/alias
       (let ((node (or (org-roam-node-from-id title-or-id)
                       (org-roam-node-from-title-or-alias title-or-id)
                       (user-error "No node with title or id \"%s\"" title-or-id))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node)))))
    ;; Setup `org-id' for the current capture target and return it back to the
    ;; caller.
    (save-excursion
      (goto-char p)
      (if-let ((id (org-entry-get p "ID")))
          (setf (org-roam-node-id org-roam-capture--node) id)
        (org-entry-put p "ID" (org-roam-node-id org-roam-capture--node)))
      (prog1
          (org-id-get)
        (run-hooks 'org-roam-capture-new-node-hook)))))

(defun org-roam-capture--get-target ()
  "Get the current capture :target for the capture template in use."
  (or (org-roam-capture--get :target)
      (user-error "Template needs to specify `:target'")))

(defun org-roam-capture--target-truepath (path)
  "From PATH get the correct path to the current capture target and return it.
PATH is a string that can optionally contain templated text in
it."
  (or (org-roam-node-file org-roam-capture--node)
      (thread-first
        path
        (org-roam-capture--fill-template)
        (string-trim)
        (expand-file-name org-roam-directory))))

(defun org-roam-capture--new-file-p (path)
  "Return t if PATH is for a new file with no visiting buffer."
  (not (or (file-exists-p path)
           (org-find-base-buffer-visiting path))))

(defun org-roam-capture-find-or-create-olp (olp)
  "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
  (let* ((level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading olp)
       (setq heading (org-roam-capture--fill-template heading))
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (let (org-insert-heading-respect-content)
             (org-insert-heading nil nil t))
           (unless (= lmax 1)
             (dotimes (_ level) (org-do-demote)))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

(defun org-roam-capture--adjust-point-for-capture-type (&optional pos)
  "Reposition the point for template insertion dependently on the capture type.
Return the newly adjusted position of `point'.

POS is the current position of point (an integer) inside the
currently active capture buffer, where the adjustment should
start to begin from. If it's nil, then it will default to
the current value of `point'."
  (or pos (setq pos (point)))
  (goto-char pos)
  (let ((location-type (if (= pos 1) 'beginning-of-file 'heading-at-point)))
    (and (eq location-type 'heading-at-point)
         (cl-assert (org-at-heading-p)))
    (pcase (org-capture-get :type)
      (`plain
       (cl-case location-type
         (beginning-of-file
          (if (org-capture-get :prepend)
              (let ((el (org-element-at-point)))
                (while (and (not (eobp))
                            (memq (org-element-type el)
                                  '(drawer property-drawer keyword comment comment-block horizontal-rule)))
                  (goto-char (org-element-property :end el))
                  (setq el (org-element-at-point))))
            (goto-char (org-entry-end-position))))
         (heading-at-point
          (if (org-capture-get :prepend)
              (org-end-of-meta-data t)
            (goto-char (org-entry-end-position))))))))
  (point))

;;; Capture implementation
(add-hook 'org-roam-capture-preface-hook #'org-roam-capture--try-capture-to-ref-h)
(defun org-roam-capture--try-capture-to-ref-h ()
  "Try to capture to an existing node that match the ref."
  (when-let ((node (and (plist-get org-roam-capture--info :ref)
                        (org-roam-node-from-ref
                         (plist-get org-roam-capture--info :ref)))))
    (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
    (goto-char (org-roam-node-point node))
    (widen)
    (org-roam-node-id node)))

(add-hook 'org-roam-capture-new-node-hook #'org-roam-capture--insert-captured-ref-h)
(defun org-roam-capture--insert-captured-ref-h ()
  "Insert the ref if any."
  (when-let ((ref (plist-get org-roam-capture--info :ref)))
    (org-roam-ref-add ref)))

;;;; Finalizers
(add-hook 'org-capture-prepare-finalize-hook #'org-roam-capture--install-finalize-h)
(defun org-roam-capture--install-finalize-h ()
  "Install `org-roam-capture--finalize' if the capture is an Org-roam capture."
  (when (org-roam-capture-p)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize)))

(defun org-roam-capture--finalize ()
  "Finalize the `org-roam-capture' process."
  (if org-note-abort
      (when-let ((new-file (org-roam-capture--get :new-file))
                 (_ (yes-or-no-p "Delete file for aborted capture?")))
        (when (find-buffer-visiting new-file)
          (kill-buffer (find-buffer-visiting new-file)))
        (delete-file new-file))
    (when-let* ((buffer (plist-get org-capture-plist :buffer))
                (file (buffer-file-name buffer)))
      (org-id-add-location (org-roam-capture--get :id) file))
    (when-let* ((finalize (org-roam-capture--get :finalize))
                (org-roam-finalize-fn (intern (concat "org-roam-capture--finalize-"
                                                      (symbol-name finalize)))))
      (if (functionp org-roam-finalize-fn)
          (funcall org-roam-finalize-fn)
        (funcall finalize))))
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-capture--finalize))

(defun org-roam-capture--finalize-find-file ()
  "Visit the buffer after Org-capture is done.
This function is to be called in the Org-capture finalization process.
ID is unused."
  (switch-to-buffer (org-capture-get :buffer)))

(defun org-roam-capture--finalize-insert-link ()
  "Insert a link to ID into the buffer where Org-capture was called.
ID is the Org id of the newly captured content.
This function is to be called in the Org-capture finalization process."
  (when-let* ((mkr (org-roam-capture--get :call-location))
              (buf (marker-buffer mkr)))
    (with-current-buffer buf
      (when-let ((region (org-roam-capture--get :region)))
        (delete-region (car region) (cdr region))
        (set-marker (car region) nil)
        (set-marker (cdr region) nil))
      (let* ((id (org-roam-capture--get :id))
             (description (org-roam-capture--get :link-description))
             (link (org-link-make-string (concat "id:" id)
                                         description)))
        (if (eq (point) (marker-position mkr))
            (insert link)
          (org-with-point-at mkr
            (insert link)))
        (run-hook-with-args 'org-roam-post-node-insert-hook
                            id
                            description)))))

;;;; Processing of the capture templates
(defun org-roam-capture--fill-template (template &optional ensure-newline)
  "Expand TEMPLATE and return it.
It expands ${var} occurrences in TEMPLATE, and then runs
org-capture's template expansion.
When ENSURE-NEWLINE, always ensure there's a newline behind."
  (let* ((template (if (functionp template)
                       (funcall template)
                     template))
         (template-whitespace-content (org-roam-whitespace-content template)))
    (setq template
          (org-roam-format-template
           template
           (lambda (key default-val)
             (let ((fn (intern key))
                   (node-fn (intern (concat "org-roam-node-" key)))
                   (ksym (intern (concat ":" key))))
               (cond
                ((fboundp fn)
                 (funcall fn org-roam-capture--node))
                ((fboundp node-fn)
                 (funcall node-fn org-roam-capture--node))
                ((plist-get org-roam-capture--info ksym)
                 (plist-get org-roam-capture--info ksym))
                (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                     (plist-put org-roam-capture--info ksym r)
                     r)))))))
    ;; WARNING:
    ;; `org-capture-fill-template' fills the template, but post-processes whitespace such that the resultant
    ;; template does not start with any whitespace, and only ends with a single newline
    ;;
    ;; Instead, we restore the whitespace in the original template.
    (setq template (replace-regexp-in-string "[\n]*\\'" "" (org-capture-fill-template template)))
    (when (and ensure-newline
               (string-equal template-whitespace-content ""))
      (setq template-whitespace-content "\n"))
    (setq template (concat template template-whitespace-content))
    template))

(defun org-roam-capture--convert-template (template &optional props)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax.
PROPS is a plist containing additional Org-roam specific
properties to be added to the template."
  (pcase template
    (`(,_key ,_desc)
     template)
    ((or `(,key ,desc ,type ignore ,body . ,rest)
         `(,key ,desc ,type (function ignore) ,body . ,rest)
         `(,key ,desc ,type ,body . ,rest))
     (setq rest (append rest props))
     (let (org-roam-plist options)
       (while rest
         (let* ((key (pop rest))
                (val (pop rest))
                (custom (member key org-roam-capture--template-keywords)))
           (when (and custom
                      (not val))
             (user-error "Invalid capture template format: %s\nkey %s cannot be nil" template key))
           (if custom
               (setq org-roam-plist (plist-put org-roam-plist key val))
             (setq options (plist-put options key val)))))
       (append `(,key ,desc ,type #'org-roam-capture--prepare-buffer ,body)
               options
               (list :org-roam org-roam-plist))))
    (_
     (signal 'invalid-template template))))


(provide 'org-roam-capture)

;;; org-roam-capture.el ends here
