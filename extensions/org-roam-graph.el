;;; org-roam-graph.el --- Basic graphing functionality for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-roam "2.1"))

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
;; This extension implements capability to build and generate graphs in Org-roam
;; with the help of Graphviz.
;;
;;; Code:
(require 'xml) ;xml-escape-string
(require 'org-roam)

;;; Options
(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Method to view the org-roam graph.
It may be one of the following:
  - a string representing the path to the executable for viewing the graph.
  - a function accepting a single argument: the graph file path.
  - nil uses `view-file' to view the graph."
  :type '(choice
          (string   :tag "Path to executable")
          (function :tag "Function to display graph" eww-open-file)
          (const    :tag "view-file"))
  :group 'org-roam)

(defcustom org-roam-graph-executable "dot"
  "Path to graphing executable, or its name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-filetype "svg"
  "File type to generate when producing graphs."
  :type 'string
  :group 'org-roam)


(defcustom org-roam-graph-extra-config nil
  "Extra options passed to graphviz.
Example:
 '((\"rankdir\" . \"LR\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-edge-extra-config nil
  "Extra edge options passed to graphviz.
Example:
 '((\"dir\" . \"back\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-node-extra-config
  '(("id" . (("style"      . "bold,rounded,filled")
             ("fillcolor"  . "#EEEEEE")
             ("color"      . "#C9C9C9")
             ("fontcolor"  . "#111111")))
    ("http" . (("style"      . "rounded,filled")
               ("fillcolor"  . "#EEEEEE")
               ("color"      . "#C9C9C9")
               ("fontcolor"  . "#0A97A6")))
    ("https" . (("style"      . "rounded,filled")
                ("fillcolor"  . "#EEEEEE")
                ("color"      . "#C9C9C9")
                ("fontcolor"  . "#0A97A6"))))
  "Extra options for graphviz nodes."
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-link-hidden-types
  '("file")
  "What sort of links to hide from the Org-roam graph."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-graph-max-title-length 100
  "Maximum length of titles in graph nodes."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-graph-shorten-titles 'truncate
  "Determines how long titles appear in graph nodes.
Recognized values are the symbols `truncate' and `wrap', in which
cases the title will be truncated or wrapped, respectively, if it
is longer than `org-roam-graph-max-title-length'.

All other values including nil will have no effect."
  :type '(choice
          (const :tag "truncate" truncate)
          (const :tag "wrap" wrap)
          (const :tag "no" nil))
  :group 'org-roam)

(defcustom org-roam-graph-link-builder 'org-roam-org-protocol-link-builder
  "Function used to build the Org-roam graph links.
Given a node name, return a string to be used for the link fed to
the graph generation utility."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-graph-generation-hook nil
  "Functions to run after the graph has been generated.
Each function is called with two arguments: the filename
containing the graph generation tool, and the generated graph."
  :type 'hook
  :group 'org-roam)

(defun org-roam-org-protocol-link-builder (node)
  "Default org-roam link builder.  Generate an org-protocol link using NODE."
  (concat "org-protocol://roam-node?node="
          (url-hexify-string (org-roam-node-id node))))

;;; Interactive command
;;;###autoload
(defun org-roam-graph (&optional arg node)
  "Build and possibly display a graph for NODE.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for NODE.
  - `\\[universal-argument]' N   show the graph for NODE limiting nodes to N steps."
  (interactive
   (list current-prefix-arg
         (and current-prefix-arg
              (org-roam-node-at-point 'assert))))
  (let ((graph (cl-typecase arg
                 (null (org-roam-graph--dot nil 'all-nodes))
                 (cons (org-roam-graph--dot (org-roam-graph--connected-component
                                             (org-roam-node-id node) 0)))
                 (integer (org-roam-graph--dot (org-roam-graph--connected-component
                                                (org-roam-node-id node) (abs arg)))))))
    (org-roam-graph--build graph #'org-roam-graph--open)))

;;; Generation and Build process
(defun org-roam-graph--build (graph &optional callback)
  "Generate the GRAPH, and execute CALLBACK when process exits successfully.
CALLBACK is passed the graph file as its sole argument."
  (unless (stringp org-roam-graph-executable)
    (user-error "`org-roam-graph-executable' is not a string"))
  (unless (executable-find org-roam-graph-executable)
    (user-error (concat "Cannot find executable \"%s\" to generate the graph.  "
                        "Please adjust `org-roam-graph-executable'")
                org-roam-graph-executable))
  (let* ((temp-dot   (make-temp-file "graph." nil ".dot" graph))
         (temp-graph (make-temp-file "graph." nil (concat "." org-roam-graph-filetype))))
    (org-roam-message "building graph")
    (make-process
     :name "*org-roam-graph*"
     :buffer " *org-roam-graph*"
     :command `(,org-roam-graph-executable ,temp-dot "-T" ,org-roam-graph-filetype "-o" ,temp-graph)
     :sentinel (when callback
                 (lambda (process _event)
                   (when (= 0 (process-exit-status process))
                     (progn (funcall callback temp-graph)
                            (run-hook-with-args 'org-roam-graph-generation-hook temp-dot temp-graph))))))))

(defun org-roam-graph--dot (&optional edges all-nodes)
  "Build the graphviz given the EDGES of the graph.
If ALL-NODES, include also nodes without edges."
  (let ((org-roam-directory-temp org-roam-directory)
        (nodes-table (make-hash-table :test #'equal))
        (seen-nodes (list))
        (edges (or edges (org-roam-db-query [:select :distinct [source dest type] :from links]))))
    (pcase-dolist (`(,id ,file ,title)
                   (org-roam-db-query [:select [id file title] :from nodes]))
      (puthash id (org-roam-node-create :file file :id id :title title) nodes-table))
    (with-temp-buffer
      (setq-local org-roam-directory org-roam-directory-temp)
      (insert "digraph \"org-roam\" {\n")
      (dolist (option org-roam-graph-extra-config)
        (insert (org-roam-graph--dot-option option) ";\n"))
      (insert (format " edge [%s];\n"
                      (mapconcat (lambda (var)
                                   (org-roam-graph--dot-option var nil "\""))
                                 org-roam-graph-edge-extra-config
                                 ",")))
      (pcase-dolist (`(,source ,dest ,type) edges)
        (unless (member type org-roam-graph-link-hidden-types)
          (pcase-dolist (`(,node ,node-type) `((,source "id")
                                               (,dest ,type)))
            (unless (member node seen-nodes)
              (insert (org-roam-graph--format-node
                       (or (gethash node nodes-table) node) node-type))
              (push node seen-nodes)))
          (insert (format "  \"%s\" -> \"%s\";\n"
                          (xml-escape-string source)
                          (xml-escape-string dest)))))
      (when all-nodes
        (maphash (lambda (id node)
                   (unless (member id seen-nodes)
                     (insert (org-roam-graph--format-node node "id"))))
                 nodes-table))
      (insert "}")
      (buffer-string))))

(defun org-roam-graph--connected-component (id distance)
  "Return the edges for all nodes reachable from/connected to ID.
DISTANCE is the maximum distance away from the root node."
  (let* ((query
          (if (= distance 0)
              "
WITH RECURSIVE
  links_of(source, dest) AS
  (SELECT source, dest FROM links UNION
   SELECT dest, source FROM links),
   connected_component(source) AS
  (SELECT dest FROM links_of WHERE source = $s1 UNION
   SELECT dest FROM links_of JOIN connected_component USING(source))
SELECT DISTINCT source, dest, type FROM links
WHERE source IN connected_component OR dest IN connected_component;"
            "
WITH RECURSIVE
  links_of(source, dest) AS
  (SELECT source, dest FROM links UNION
   SELECT dest, source FROM links),
  connected_component(source, trace) AS
  (VALUES ($s1 , json_array($s1)) UNION
   SELECT lo.dest, json_insert(cc.trace, '$[' || json_array_length(cc.trace) || ']', lo.dest) FROM
   connected_component AS cc JOIN links_of AS lo USING(source)
   WHERE (
    -- Avoid cycles by only visiting each node once.
    (SELECT count(*) FROM json_each(cc.trace) WHERE json_each.value == lo.dest) == 0
    -- Note: BFS is cut off early here.
    AND json_array_length(cc.trace) < $s2)),
  nodes(source) as (SELECT DISTINCT source
   FROM connected_component GROUP BY source ORDER BY min(json_array_length(trace)))
SELECT DISTINCT source, dest, type FROM links WHERE source IN nodes OR dest IN nodes;")))
    (org-roam-db-query query id distance)))

(defun org-roam-graph--dot-option (option &optional wrap-key wrap-val)
  "Return dot string of form KEY=VAL for OPTION cons.
If WRAP-KEY is non-nil it wraps the KEY.
If WRAP-VAL is non-nil it wraps the VAL."
  (concat wrap-key (car option) wrap-key
          "="
          wrap-val (cdr option) wrap-val))

(defun org-roam-graph--format-node (node type)
  "Return a graphviz NODE with TYPE.
Handles both Org-roam nodes, and string nodes (e.g. urls)."
  (let (node-id node-properties)
    (if (org-roam-node-p node)
        (let* ((title (org-roam-quote-string (org-roam-node-title node)))
               (shortened-title
                (org-roam-quote-string
                 (pcase org-roam-graph-shorten-titles
                   (`truncate (truncate-string-to-width title org-roam-graph-max-title-length nil nil "..."))
                   (`wrap (s-word-wrap org-roam-graph-max-title-length title))
                   (_ title)))))
          (setq node-id (org-roam-node-id node)
                node-properties `(("label"   . ,shortened-title)
                                  ("URL"     . ,(funcall org-roam-graph-link-builder node))
                                  ("tooltip" . ,(xml-escape-string title)))))
      (setq node-id node
            node-properties (append `(("label" . ,(concat type ":" node)))
                                    (when (member type (list "http" "https"))
                                      `(("URL" . ,(xml-escape-string (concat type ":" node))))))))
    (format "\"%s\" [%s];\n"
            node-id
            (mapconcat (lambda (n)
                         (org-roam-graph--dot-option n nil "\""))
                       (append (cdr (assoc type org-roam-graph-node-extra-config))
                               node-properties) ","))))

(defun org-roam-graph--open (file)
  "Open FILE using `org-roam-graph-viewer' with `view-file' as a fallback."
  (pcase org-roam-graph-viewer
    ((pred stringp)
     (if (executable-find org-roam-graph-viewer)
         (condition-case err
             (call-process org-roam-graph-viewer nil 0 nil file)
           (error (user-error "Failed to open org-roam graph: %s" err)))
       (user-error "Executable not found: \"%s\"" org-roam-graph-viewer)))
    ((pred functionp) (funcall org-roam-graph-viewer file))
    ('nil (view-file file))
    (_ (signal 'wrong-type-argument `((functionp stringp null) ,org-roam-graph-viewer)))))


(provide 'org-roam-graph)

;;; org-roam-graph.el ends here
