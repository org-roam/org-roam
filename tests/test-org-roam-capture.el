;;; test-org-roam-capture.el --- Tests for Org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'org-roam)

(describe "org-roam-capture--fill-template"
  (it "fills template without newline"
    (expect
     (org-roam-capture--fill-template "foo")
     :to-equal "foo"))

  (it "fills template ensuring newline"
    (expect
     (org-roam-capture--fill-template "foo" 'ensure-newline)
     :to-equal "foo\n"))

  (it "fills template with newline"
    (expect
     (org-roam-capture--fill-template "foo\n")
     :to-equal "foo\n"))

  (it "fills template with two newlines"
    (expect
     (org-roam-capture--fill-template "foo\n\n")
     :to-equal "foo\n\n")
    (expect
     (org-roam-capture--fill-template "foo\n\t\n")
     :to-equal "foo\n\t\n"))

  (it "fills template without deleting newlines in its body"
    (expect
     (org-roam-capture--fill-template "foo\n\n\nbar\n\n")
     :to-equal "foo\n\n\nbar\n\n"))

  (it "expands templates when it's a function"
    (expect
     (org-roam-capture--fill-template (lambda () "foo"))
     :to-equal "foo")))

;; Regression tests for capture regressions fixed by revert b2634a1.
;; These cover issues #2584, #2582, #2551, and #2550.

(describe "org-roam-capture--convert-template"
  ;; Regression: #2550 — entry-type templates with third-party packages
  ;; (e.g. org-roam-bibtex) produced "Wrong type argument: stringp, nil"
  ;; because the converted template had a nil body.
  (it "converts entry-type template to valid org-capture format"
    (let* ((org-roam-capture--node (org-roam-node-create :title "Test"))
           (org-roam-capture--info nil)
           (template '("d" "default" entry "* %?"
                       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                          "#+title: ${title}\n")
                       :unnarrowed t))
           (result (org-roam-capture--convert-template template)))
      ;; The converted template must have a string or function body (element 4),
      ;; never nil.
      (expect (nth 4 result) :not :to-be nil)
      ;; Must be a string (the original template text)
      (expect (nth 4 result) :to-equal "* %?")
      ;; The capture type must be preserved
      (expect (nth 2 result) :to-equal 'entry)
      ;; The target function must be org-roam-capture--prepare-buffer
      (expect (nth 3 result) :to-equal '(function org-roam-capture--prepare-buffer))))

  (it "converts plain-type template to valid org-capture format"
    (let* ((org-roam-capture--node (org-roam-node-create :title "Test"))
           (org-roam-capture--info nil)
           (template '("d" "default" plain "%?"
                       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                          "#+title: ${title}\n")
                       :unnarrowed t))
           (result (org-roam-capture--convert-template template)))
      (expect (nth 4 result) :to-equal "%?")
      (expect (nth 2 result) :to-equal 'plain)
      (expect (nth 3 result) :to-equal '(function org-roam-capture--prepare-buffer))))

  (it "preserves all non-org-roam properties in converted template"
    (let* ((template '("d" "default" entry "* %?"
                       :target (file+head "test.org" "#+title: Test\n")
                       :unnarrowed t
                       :prepend t
                       :empty-lines 1))
           (result (org-roam-capture--convert-template template)))
      ;; Non-org-roam properties should be preserved in the plist tail
      (expect (plist-get (nthcdr 5 result) :unnarrowed) :to-be t)
      (expect (plist-get (nthcdr 5 result) :prepend) :to-be t)
      (expect (plist-get (nthcdr 5 result) :empty-lines) :to-equal 1))))

(describe "org-roam-capture--setup-target-location ID placement"
  ;; These tests verify that the ID (PROPERTIES drawer) is always created at
  ;; the file-level target position, not at the captured entry heading.
  ;;
  ;; Regressions #2582, #2551: Using entry-type dailies capture templates with
  ;; file+head target caused the ID to be placed on the entry heading instead
  ;; of at the file level.
  ;;
  ;; Regression #2584: Using plain-type capture with file+head target caused
  ;; the PROPERTIES drawer to appear at the end of the node instead of the
  ;; beginning.

  :var (temp-dir)

  (before-each
    (setq temp-dir (make-temp-file "org-roam-test" t))
    (setq org-roam-directory temp-dir)
    (setq org-roam-db-location (expand-file-name "org-roam.db" temp-dir)))

  (after-each
    (delete-directory temp-dir t))

  (it "places ID at file level for file target"
    (let* ((test-file (expand-file-name "test.org" temp-dir))
           (test-id (org-id-new))
           (org-roam-capture--node (org-roam-node-create :id test-id))
           (org-roam-capture--info nil)
           (org-capture-plist nil))
      ;; Mock org-roam-capture--get and org-capture-get to simulate a
      ;; plain-type capture with file target
      (cl-letf (((symbol-function 'org-roam-capture--get)
                 (lambda (prop)
                   (pcase prop
                     (:target `(file ,test-file))
                     (_ nil))))
                ((symbol-function 'org-capture-get)
                 (lambda (prop &rest _) nil)))
        (with-current-buffer (find-file-noselect test-file)
          (org-roam-capture--setup-target-location)
          ;; ID should be at the beginning of the file
          (goto-char (point-min))
          (expect (org-entry-get (point-min) "ID") :to-equal test-id)
          (kill-buffer)))))

  (it "places ID at file level for file+head target on new file"
    ;; Regression: #2582, #2551, #2584
    (let* ((test-file (expand-file-name "test-fh.org" temp-dir))
           (test-id (org-id-new))
           (org-roam-capture--node (org-roam-node-create :id test-id
                                                         :title "Test Node"))
           (org-roam-capture--info nil)
           (org-capture-plist nil))
      (cl-letf (((symbol-function 'org-roam-capture--get)
                 (lambda (prop)
                   (pcase prop
                     (:target `(file+head ,test-file "#+title: Test Node\n"))
                     (_ nil))))
                ((symbol-function 'org-capture-get)
                 (lambda (prop &rest _) nil)))
        (with-current-buffer (find-file-noselect test-file)
          (org-roam-capture--setup-target-location)
          ;; ID must be at point-min, not somewhere else in the buffer
          (expect (org-entry-get (point-min) "ID") :to-equal test-id)
          ;; The PROPERTIES drawer must come before the #+title
          (goto-char (point-min))
          (let ((content (buffer-string)))
            (expect (string-match ":PROPERTIES:" content) :to-be 0))
          (kill-buffer)))))

  (it "returns ID from setup-target-location"
    (let* ((test-file (expand-file-name "test-ret.org" temp-dir))
           (test-id (org-id-new))
           (org-roam-capture--node (org-roam-node-create :id test-id))
           (org-roam-capture--info nil)
           (org-capture-plist nil)
           returned-id)
      (cl-letf (((symbol-function 'org-roam-capture--get)
                 (lambda (prop)
                   (pcase prop
                     (:target `(file ,test-file))
                     (_ nil))))
                ((symbol-function 'org-capture-get)
                 (lambda (prop &rest _) nil)))
        (with-current-buffer (find-file-noselect test-file)
          (setq returned-id (org-roam-capture--setup-target-location))
          ;; Must return the ID string
          (expect returned-id :to-equal test-id)
          (kill-buffer)))))

  (it "reuses existing ID when file already has one"
    (let* ((test-file (expand-file-name "test-existing.org" temp-dir))
           (existing-id "existing-test-id-12345")
           (new-id (org-id-new))
           (org-roam-capture--node (org-roam-node-create :id new-id))
           (org-roam-capture--info nil)
           (org-capture-plist nil))
      ;; Create file with existing ID
      (with-temp-file test-file
        (insert ":PROPERTIES:\n:ID:       " existing-id "\n:END:\n#+title: Existing\n"))
      (cl-letf (((symbol-function 'org-roam-capture--get)
                 (lambda (prop)
                   (pcase prop
                     (:target `(file ,test-file))
                     (_ nil))))
                ((symbol-function 'org-capture-get)
                 (lambda (prop &rest _) nil)))
        (with-current-buffer (find-file-noselect test-file)
          (let ((returned-id (org-roam-capture--setup-target-location)))
            ;; Should reuse existing ID, not create a new one
            (expect returned-id :to-equal existing-id)
            ;; The node's ID should be updated to match the existing ID
            (expect (org-roam-node-id org-roam-capture--node) :to-equal existing-id))
          (kill-buffer))))))

(describe "org-roam-capture--adjust-point-for-capture-type"
  ;; Regression: #2584 — PROPERTIES drawer with ID was inserted at the end
  ;; of the generated node instead of at the beginning, because point
  ;; adjustment did not properly handle the beginning-of-file vs
  ;; heading-at-point distinction.

  (it "positions point after metadata for plain type at beginning of file"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: Test\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-capture-get)
                 (lambda (prop &rest _)
                   (pcase prop
                     (:type 'plain)
                     (:prepend t)
                     (_ nil)))))
        (org-roam-capture--adjust-point-for-capture-type 1)
        ;; Point should be after the properties drawer and keywords,
        ;; not at the beginning
        (expect (point) :to-be-greater-than 1))))

  (it "positions point at end for plain type without prepend at beginning of file"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: Test\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-capture-get)
                 (lambda (prop &rest _)
                   (pcase prop
                     (:type 'plain)
                     (_ nil)))))
        (org-roam-capture--adjust-point-for-capture-type 1)
        ;; Without :prepend, point should be at the end of the entry
        (expect (point) :to-equal (point-max)))))

  (it "does not move point for entry type"
    ;; Entry type is handled by org-capture itself; org-roam should
    ;; not interfere with its positioning.
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: Test\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-capture-get)
                 (lambda (prop &rest _)
                   (pcase prop
                     (:type 'entry)
                     (_ nil)))))
        (let ((pos-before (point)))
          (org-roam-capture--adjust-point-for-capture-type 1)
          ;; Entry type should not adjust point (it's not in the pcase)
          (expect (point) :to-equal pos-before)))))

  (it "handles heading-at-point location for plain type with prepend"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:ID: file-id\n:END:\n#+title: Test\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\nSome content\n")
      ;; Go to the heading
      (goto-char (point-min))
      (re-search-forward "^\\* Heading")
      (beginning-of-line)
      (let ((heading-pos (point)))
        (cl-letf (((symbol-function 'org-capture-get)
                   (lambda (prop &rest _)
                     (pcase prop
                       (:type 'plain)
                       (:prepend t)
                       (_ nil)))))
          (org-roam-capture--adjust-point-for-capture-type heading-pos)
          ;; Should be after the heading's metadata
          (expect (point) :to-be-greater-than heading-pos))))))

(provide 'test-org-roam-capture)

;;; test-org-roam-capture.el ends here
