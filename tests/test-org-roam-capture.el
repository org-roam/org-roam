;;; test-org-roam-capture.el --- Tests for Org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>

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

(defun org-roam-capture-test--finalize-capture (node templates &optional text)
  "Capture NODE with TEMPLATES, optionally inserting TEXT at point."
  (let ((org-capture-bookmark nil)
        (org-id-locations-file (expand-file-name ".org-id-locations"
                                                 org-roam-directory)))
    (org-roam-capture- :node node :templates templates :keys "d")
    (when text
      (insert text))
    (org-capture-finalize)))

(defun org-roam-capture-test--file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

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
      (pcase result
        (`("d" "default" entry (function org-roam-capture--prepare-buffer) "* %?" . ,_)
         (expect t :to-be t))
        (_ (expect result :to-equal 'converted-entry-template)))))

  (it "converts plain-type template to valid org-capture format"
    (let* ((org-roam-capture--node (org-roam-node-create :title "Test"))
           (org-roam-capture--info nil)
           (template '("d" "default" plain "%?"
                       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                          "#+title: ${title}\n")
                       :unnarrowed t))
           (result (org-roam-capture--convert-template template)))
      (pcase result
        (`("d" "default" plain (function org-roam-capture--prepare-buffer) "%?" . ,_)
         (expect t :to-be t))
        (_ (expect result :to-equal 'converted-plain-template)))))

  (it "preserves all non-org-roam properties in converted template"
    (let* ((template '("d" "default" entry "* %?"
                       :target (file+head "test.org" "#+title: Test\n")
                       :unnarrowed t
                       :prepend t
                       :empty-lines 1))
           (result (org-roam-capture--convert-template template)))
      (pcase result
        (`("d" "default" entry (function org-roam-capture--prepare-buffer)
           "* %?" . ,options)
         (expect (plist-get options :unnarrowed) :to-be t)
         (expect (plist-get options :prepend) :to-be t)
         (expect (plist-get options :empty-lines) :to-equal 1))
        (_ (expect result :to-equal 'converted-template-with-options))))))

(describe "org-roam-capture ID placement regressions"
  :var (temp-dir)

  (before-each
    (setq temp-dir (make-temp-file "org-roam-test" t))
    (setq org-roam-directory temp-dir)
    (setq org-roam-db-location (expand-file-name "org-roam.db" temp-dir)))

  (after-each
    (delete-directory temp-dir t))

  (it "places the ID at the beginning of a plain file+head capture"
    (let* ((test-file (expand-file-name "test.org" temp-dir))
           (test-id "test-id-plain-file-head")
           (node (org-roam-node-create :id test-id :title "Test Node"))
           (templates `(("d" "default" plain "%?"
                         :target (file+head ,test-file
                                            "#+title: Test Node\n#+category: Test Node\n\n* Timeline\n")
                         :unnarrowed t))))
      (org-roam-capture-test--finalize-capture node templates "Captured")
      (expect (org-roam-capture-test--file-string test-file)
              :to-equal
              (concat ":PROPERTIES:\n"
                      ":ID:       " test-id "\n"
                      ":END:\n"
                      "#+title: Test Node\n"
                      "#+category: Test Node\n"
                      "Captured\n"
                      "* Timeline\n"))))

  (it "places the ID at file level for entry file+head capture"
    (let* ((test-file (expand-file-name "test-fh.org" temp-dir))
           (test-id "test-id-entry-file-head")
           (node (org-roam-node-create :id test-id :title "Daily"))
           (templates `(("d" "default" entry "* %?"
                         :target (file+head ,test-file "#+title: Daily\n")
                         :unnarrowed t))))
      (org-roam-capture-test--finalize-capture node templates "Captured")
      (let ((content (org-roam-capture-test--file-string test-file)))
        (expect content
                :to-equal
                (concat ":PROPERTIES:\n"
                        ":ID:       " test-id "\n"
                        ":END:\n"
                        "#+title: Daily\n"
                        "* Captured\n"))
        (expect (string-match-p "^\\* Captured\n:PROPERTIES:" content)
                :to-equal nil))))

  (it "places the ID at file level for entry file+head+olp capture"
    (let* ((test-file (expand-file-name "test-fholp.org" temp-dir))
           (test-id "test-id-entry-file-head-olp")
           (node (org-roam-node-create :id test-id :title "Daily"))
           (templates `(("d" "timestamped note" entry "* %?"
                         :target (file+head+olp ,test-file
                                                "#+title: Daily\n"
                                                ("Minutes"))
                         :unnarrowed t))))
      (org-roam-capture-test--finalize-capture node templates "Captured")
      (let ((content (org-roam-capture-test--file-string test-file)))
        (expect content
                :to-equal
                (concat ":PROPERTIES:\n"
                        ":ID:       " test-id "\n"
                        ":END:\n"
                        "#+title: Daily\n"
                        "* Minutes\n"
                        "** Captured\n"))
        (expect (string-match-p "^\\*\\* Captured\n:PROPERTIES:" content)
                :to-equal nil))))

  (it "reuses existing ID when file already has one"
    (let* ((test-file (expand-file-name "test-existing.org" temp-dir))
           (existing-id "existing-test-id-12345")
           (new-id "new-test-id-12345")
           (node (org-roam-node-create :id new-id))
           (templates `(("d" "default" plain "%?"
                         :target (file ,test-file)
                         :unnarrowed t))))
      (with-temp-file test-file
        (insert ":PROPERTIES:\n:ID:       " existing-id "\n:END:\n#+title: Existing\n"))
      (org-roam-capture-test--finalize-capture node templates "Captured")
      (expect (org-roam-node-id node) :to-equal existing-id)
      (expect (org-roam-capture-test--file-string test-file)
              :to-equal
              (concat ":PROPERTIES:\n"
                      ":ID:       " existing-id "\n"
                      ":END:\n"
                      "#+title: Existing\n"
                      "Captured\n")))))

(provide 'test-org-roam-capture)

;;; test-org-roam-capture.el ends here
