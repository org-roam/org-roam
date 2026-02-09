;;; test-org-roam-mode.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(defun test--make-magit-section (type start &optional children)
  "Create a `magit-section' with TYPE, START position, and optional CHILDREN."
  (let ((section (make-instance 'magit-section)))
    (oset section type type)
    (oset section start start)
    (oset section children children)
    section))

(defun test--make-node-section (start node &optional children)
  "Create an `org-roam-node-section' at START with NODE and optional CHILDREN."
  (let ((section (make-instance 'org-roam-node-section)))
    (oset section type 'org-roam-node-section)
    (oset section start start)
    (oset section node node)
    (oset section children children)
    section))

(defun test--make-preview-section (start file point)
  "Create an `org-roam-preview-section' at START with FILE and POINT."
  (let ((section (make-instance 'org-roam-preview-section)))
    (oset section type 'org-roam-preview-section)
    (oset section start start)
    (oset section file file)
    (oset section point point)
    (oset section children nil)
    section))

(defun test--make-grep-section (start file row col)
  "Create an `org-roam-grep-section' at START with FILE, ROW, and COL."
  (let ((section (make-instance 'org-roam-grep-section)))
    (oset section type 'org-roam-grep-section)
    (oset section start start)
    (oset section file file)
    (oset section row row)
    (oset section col col)
    (oset section children nil)
    section))

(describe "org-roam-unlinked-references--rg-command"
  (before-each
    ;; the space in the directory is on purpose
    (setq org-roam-directory "/tmp/org roam"))

  (it "returns the correct rg command for unlinked references"
    (expect (org-roam-unlinked-references--rg-command '("foo" "bar") "/tmp/regex")
            :to-equal
            "rg --follow --only-matching --vimgrep --pcre2 --ignore-case --glob \"*.org\" --glob \"*.org.gpg\" --glob \"*.org.age\" --file /tmp/regex /tmp/org\\ roam")))

(describe "org-roam-mode-imenu-create-index"
  (it "returns nil when magit-root-section is nil"
    (with-temp-buffer
      (org-roam-mode)
      (let ((magit-root-section nil))
        (expect (org-roam-mode-imenu-create-index) :to-be nil))))

  (it "returns empty list when root section has no children"
    (with-temp-buffer
      (org-roam-mode)
      (let ((magit-root-section (test--make-magit-section 'org-roam 1 nil)))
        (expect (org-roam-mode-imenu-create-index) :to-equal nil))))

  (it "creates nested index for backlinks section"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((preview-section (test--make-preview-section 100 "/tmp/test.org" 50))
             (mock-node (org-roam-node-create :title "Test Node"))
             (node-section (test--make-node-section 50 mock-node (list preview-section)))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10 (list node-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let ((index (org-roam-mode-imenu-create-index)))
          (expect index :not :to-be nil)
          (expect (length index) :to-equal 1)
          (expect (caar index) :to-equal "Backlinks")
          ;; Verify nested structure with markers
          (let* ((backlinks-entry (car index))
                 (section-marker (cadr backlinks-entry))
                 (node-entry (caddr backlinks-entry)))
            ;; Verify *Section* marker
            (expect (car section-marker) :to-equal "*Section*")
            (expect (numberp (cdr section-marker)) :to-be t)
            ;; Verify node entry structure
            (expect (car node-entry) :to-equal "Test Node")
            ;; Verify *Node* marker in nested node entry
            (let ((node-marker (cadr node-entry)))
              (expect (car node-marker) :to-equal "*Node*")
              (expect (numberp (cdr node-marker)) :to-be t))
            ;; Verify Preview entry
            (let ((preview-entry (caddr node-entry)))
              (expect (car preview-entry) :to-equal "Preview")
              (expect (numberp (cdr preview-entry)) :to-be t)))))))

  (it "creates nested index for reflinks section"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((mock-node (org-roam-node-create :title "Ref Node"))
             (node-section (test--make-node-section 50 mock-node nil))
             (reflinks-section (test--make-magit-section 'org-roam-reflinks 10 (list node-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list reflinks-section))))
        (setq magit-root-section root-section)
        (let ((index (org-roam-mode-imenu-create-index)))
          (expect (caar index) :to-equal "Reflinks")))))

  (it "creates nested index for unlinked-references section"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((grep-section (test--make-grep-section 100 "/tmp/test.org" 42 15))
             (unlinked-section (test--make-magit-section 'unlinked-references 10 (list grep-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list unlinked-section))))
        (setq magit-root-section root-section)
        (let ((index (org-roam-mode-imenu-create-index)))
          (expect (caar index) :to-equal "Unlinked References")))))

  (it "creates flat entry for node without preview"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((mock-node (org-roam-node-create :title "Simple Node"))
             (node-section (test--make-node-section 50 mock-node nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10 (list node-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (backlinks-entry (car index))
               (node-entry (cadr (cdr backlinks-entry))))
          (expect (car node-entry) :to-equal "Simple Node")
          (expect (numberp (cdr node-entry)) :to-be t)))))

  (it "handles nil row and col in grep entries"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((grep-section (test--make-grep-section 100 "/tmp/test.org" nil nil))
             (unlinked-section (test--make-magit-section 'unlinked-references 10 (list grep-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list unlinked-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (unlinked-entry (car index))
               (grep-entries (cdr (cdr unlinked-entry))))
          (expect (caar grep-entries) :to-equal "test.org:0:0")))))

  (it "preserves order of multiple node entries"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((node1 (org-roam-node-create :title "First Node"))
             (node2 (org-roam-node-create :title "Second Node"))
             (node3 (org-roam-node-create :title "Third Node"))
             (node-section1 (test--make-node-section 50 node1 nil))
             (node-section2 (test--make-node-section 100 node2 nil))
             (node-section3 (test--make-node-section 150 node3 nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10
                                                          (list node-section1
                                                                node-section2
                                                                node-section3)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (backlinks-entry (car index))
               (node-entries (cddr backlinks-entry)))
          (expect (length node-entries) :to-equal 3)
          (expect (car (nth 0 node-entries)) :to-equal "First Node")
          (expect (car (nth 1 node-entries)) :to-equal "Second Node")
          (expect (car (nth 2 node-entries)) :to-equal "Third Node")))))

  (it "skips nodes with nil title"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((valid-node (org-roam-node-create :title "Valid Node"))
             (nil-title-node (org-roam-node-create :title nil))
             (valid-section (test--make-node-section 50 valid-node nil))
             (nil-section (test--make-node-section 100 nil-title-node nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10
                                                          (list valid-section nil-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (backlinks-entry (car index))
               (node-entries (cddr backlinks-entry)))
          (expect (length node-entries) :to-equal 1)
          (expect (car (car node-entries)) :to-equal "Valid Node")))))

  (it "skips nodes with empty string title"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((valid-node (org-roam-node-create :title "Valid Node"))
             (empty-title-node (org-roam-node-create :title ""))
             (valid-section (test--make-node-section 50 valid-node nil))
             (empty-section (test--make-node-section 100 empty-title-node nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10
                                                          (list valid-section empty-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (backlinks-entry (car index))
               (node-entries (cddr backlinks-entry)))
          (expect (length node-entries) :to-equal 1)
          (expect (car (car node-entries)) :to-equal "Valid Node")))))

  (it "skips node sections with nil node object"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((valid-node (org-roam-node-create :title "Valid Node"))
             (valid-section (test--make-node-section 50 valid-node nil))
             (nil-node-section (test--make-node-section 100 nil nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10
                                                          (list valid-section nil-node-section)))
             (root-section (test--make-magit-section 'org-roam 1 (list backlinks-section))))
        (setq magit-root-section root-section)
        (let* ((index (org-roam-mode-imenu-create-index))
               (backlinks-entry (car index))
               (node-entries (cddr backlinks-entry)))
          (expect (length node-entries) :to-equal 1)
          (expect (car (car node-entries)) :to-equal "Valid Node")))))

  (it "creates index for mixed section types"
    (with-temp-buffer
      (org-roam-mode)
      (let* ((backlink-node (org-roam-node-create :title "Backlink Node"))
             (backlink-node-section (test--make-node-section 50 backlink-node nil))
             (backlinks-section (test--make-magit-section 'org-roam-backlinks 10
                                                          (list backlink-node-section)))
             (reflink-node (org-roam-node-create :title "Reflink Node"))
             (reflink-node-section (test--make-node-section 200 reflink-node nil))
             (reflinks-section (test--make-magit-section 'org-roam-reflinks 150
                                                         (list reflink-node-section)))
             (grep-section (test--make-grep-section 350 "/tmp/unlinked.org" 10 5))
             (unlinked-section (test--make-magit-section 'unlinked-references 300
                                                         (list grep-section)))
             (root-section (test--make-magit-section 'org-roam 1
                                                     (list backlinks-section
                                                           reflinks-section
                                                           unlinked-section))))
        (setq magit-root-section root-section)
        (let ((index (org-roam-mode-imenu-create-index)))
          (expect index :not :to-be nil)
          (expect (length index) :to-equal 3)
          (expect (car (nth 0 index)) :to-equal "Backlinks")
          (expect (car (nth 1 index)) :to-equal "Reflinks")
          (expect (car (nth 2 index)) :to-equal "Unlinked References"))))))

(provide 'test-org-roam-mode)

;;; test-org-roam-mode.el ends here
