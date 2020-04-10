;;; org-roam-graph.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.0.0-rc1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

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
;; This library provides graphing functionality for org-roam.
;;
;;; Code:
(require 'xml) ;xml-escape-string
(require 's)   ;s-truncate, s-replace
(require 'org-roam-macs)

;;;; Declarations
(defvar org-roam-directory)
(declare-function org-roam-db--ensure-built  "org-roam-db")
(declare-function org-roam-db-query          "org-roam-db")
(declare-function org-roam-db--connected-component "org-roam-db")
(declare-function org-roam--org-roam-file-p  "org-roam")
(declare-function org-roam--path-to-slug     "org-roam")

;;;; Options
(defcustom org-roam-graph-viewer (executable-find "firefox")
  "Path to executable for viewing SVG."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-executable (executable-find "dot")
  "Path to graphing executable."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-graph-extra-config nil
  "Extra options passed to graphviz.
Example:
 '((\"rankdir\" . \"LR\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-node-extra-config nil
  "Extra options for graphviz nodes.
Example:
 '((\"color\" . \"skyblue\"))"
  :type '(alist)
  :group 'org-roam)

(defcustom org-roam-graph-max-title-length 100
  "Maximum length of titles in graph nodes."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-graph-exclude-matcher nil
  "Matcher for excluding nodes from the generated graph.
Any nodes and links for file paths matching this string is
excluded from the graph.

If value is a string, the string is the only matcher.

If value is a list, all file paths matching any of the strings
are excluded."
  :type '(choice
          (string :tag "Matcher")
          (list :tag "Matchers"))
  :group 'org-roam)

(make-obsolete-variable 'org-roam-graph-node-shape  'org-roam-graph-node-extra-config "2020/04/01")
(defcustom org-roam-graph-node-shape "ellipse"
  "Shape of graph nodes."
  :type 'string
  :group 'org-roam)

;;;; Functions
(defun org-roam-graph--expand-matcher (col &optional negate where)
  "Return the exclusion regexp from `org-roam-graph-exclude-matcher'.
COL is the symbol to be matched against.  if NEGATE, add :not to sql query.
set WHERE to true if WHERE query already exists."
  (let ((matchers (cond ((null org-roam-graph-exclude-matcher)
                         nil)
                        ((stringp org-roam-graph-exclude-matcher)
                         (cons (concat "%" org-roam-graph-exclude-matcher "%") nil))
                        ((listp org-roam-graph-exclude-matcher)
                         (mapcar (lambda (m)
                                   (concat "%" m "%"))
                                 org-roam-graph-exclude-matcher))
                        (t
                         (error "Invalid org-roam-graph-exclude-matcher"))))
        res)
    (dolist (match matchers)
      (if where
          (push :and res)
        (push :where res)
        (setq where t))
      (push col res)
      (when negate
        (push :not res))
      (push :like res)
      (push match res))
    (nreverse res)))

(defun org-roam-graph--build (node-query)
  "Build the graphviz string for NODE-QUERY.
The Org-roam database titles table is read, to obtain the list of titles.
The links table is then read to obtain all directed links, and formatted
into a digraph."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [to from] :from links
              :where (and (in to selected) (in from selected))])
           (edges (org-roam-db-query edges-query)))
      (insert "digraph \"org-roam\" {\n")
      (dolist (option org-roam-graph-extra-config)
        (insert (concat (car option)
                        "="
                        (cdr option)
                        ";\n")))
      (dolist (node nodes)
        (let* ((file (xml-escape-string (car node)))
               (title (or (caadr node)
                          (org-roam--path-to-slug file)))
               (shortened-title (s-truncate org-roam-graph-max-title-length title))
               (base-node-properties (list (cons "label" (s-replace "\"" "\\\"" shortened-title))
                                           (cons "URL" (concat "org-protocol://roam-file?file="
                                                               (url-hexify-string file)))
                                           (cons "tooltip" (xml-escape-string title))))
               (node-properties (append base-node-properties
                                        org-roam-graph-node-extra-config)))
          (insert
           (format "  \"%s\" [%s];\n"
                   file
                   (->> node-properties
                        (mapcar (lambda (n)
                                  (concat (car n) "=" "\"" (cdr n) "\"")))
                        (s-join ","))))))
      (dolist (edge edges)
        (insert (format "  \"%s\" -> \"%s\";\n"
                        (xml-escape-string (car edge))
                        (xml-escape-string (cadr edge)))))
      (insert "}")
      (buffer-string))))

(defun org-roam-graph-show (&optional prefix node-query)
  "Generate and displays the Org-roam graph using `org-roam-graph-viewer'.
If PREFIX, then the graph is generated but the viewer is not invoked."
  (interactive "P")
  (declare (indent 0))
  (unless org-roam-graph-executable
    (user-error "Can't find %s executable.  Please check if it is in your path"
                org-roam-graph-executable))
  (let* ((temp-dot (expand-file-name "graph.dot" temporary-file-directory))
         (temp-graph (expand-file-name "graph.svg" temporary-file-directory))
         (node-query (or node-query `[:select [file titles]
                                      :from titles
                                      ,@(org-roam-graph--expand-matcher 'file t)]))
         (graph (org-roam-graph--build node-query)))
    (with-temp-file temp-dot
      (insert graph))
    (call-process org-roam-graph-executable nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (unless prefix
      (if (and org-roam-graph-viewer (executable-find org-roam-graph-viewer))
	        (call-process org-roam-graph-viewer nil 0 nil temp-graph)
        (view-file temp-graph)))))

(defun org-roam-graph-show-connected-component (&optional max-distance no-display)
  "Like `org-roam-graph-show', but only show nodes connected to the current entry.
If MAX-DISTANCE is non-nil, only nodes within the given number of steps are shown.
If NO-DISPLAY is non-nil, the graph is generated but the viewer is not invoked."
  (interactive "P")
  (unless (org-roam--org-roam-file-p)
    (user-error "Not in an Org-roam file"))
  (let* ((file (file-truename (buffer-file-name)))
         (files (or (if (and max-distance (>= (prefix-numeric-value max-distance) 0))
                        (org-roam-db--links-with-max-distance file max-distance)
                      (org-roam-db--connected-component file))
                    (list file)))
         (query `[:select [file titles]
                  :from titles
                  :where (in file [,@files])]))
    (org-roam-graph-show no-display query)))

(provide 'org-roam-graph)

;;; org-roam-graph.el ends here
