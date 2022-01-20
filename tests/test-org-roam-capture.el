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

(describe "org-roam-capture entry-type ID creation"
  (it "creates ID for entry-type captures"
    (let* ((temp-dir (make-temp-file "org-roam-test" t))
           (test-file (expand-file-name "test.org" temp-dir))
           (org-roam-directory temp-dir)
           (org-roam-capture--node (org-roam-node-create :id (org-id-new)))
           (org-roam-capture--info (make-hash-table :test 'equal))
           capture-id)
      (unwind-protect
          (progn
            ;; Create initial file content
            (with-temp-file test-file
              (insert "#+TITLE: Test File\n\n* Parent Heading\n:PROPERTIES:\n:ID: parent-id\n:END:\n"))

            ;; Mock org-capture context and get-target
            (cl-letf* (((symbol-function 'org-capture-get)
                        (lambda (prop)
                          (pcase prop
                            (:type 'entry)
                            (_ nil))))
                       ((symbol-function 'org-roam-capture--get-target)
                        (lambda () `(file ,test-file))))

              ;; Call the setup function
              (with-current-buffer (find-file-noselect test-file)
                (org-roam-capture--setup-target-location)
                (setq capture-id (org-roam-capture--get :id)))

              ;; Verify ID was created and stored for entry type
              (expect capture-id :not :to-be nil)
              (expect (org-roam-capture--get :new-node-p) :to-be t)))
        (delete-directory temp-dir t))))

  (it "creates ID at target for non-entry-type captures"
    (let* ((temp-dir (make-temp-file "org-roam-test" t))
           (test-file (expand-file-name "test-plain.org" temp-dir))
           (org-roam-directory temp-dir)
           (org-roam-capture--node (org-roam-node-create :id (org-id-new)))
           (org-roam-capture--info (make-hash-table :test 'equal))
           capture-id)
      (unwind-protect
          (progn
            ;; Create initial empty file
            (with-temp-file test-file
              (insert "#+TITLE: Test\n"))

            ;; Mock org-capture context for plain type
            (cl-letf* (((symbol-function 'org-capture-get)
                        (lambda (prop)
                          (pcase prop
                            (:type 'plain)
                            (_ nil))))
                       ((symbol-function 'org-roam-capture--get-target)
                        (lambda () `(file ,test-file))))

              ;; Call the setup function
              (with-current-buffer (find-file-noselect test-file)
                (org-roam-capture--setup-target-location)
                (setq capture-id (org-roam-capture--get :id)))

              ;; For non-entry types, ID should be created at the target location
              (expect capture-id :not :to-be nil)
              (expect (org-roam-capture--get :new-node-p) :to-be t)))
        (delete-directory temp-dir t)))))

(describe "org-roam-capture advice functions"
  :var ((org-roam-capture--info))

  (before-each
    (setq org-roam-capture--info (make-hash-table :test 'equal)))

  (it "org-roam-capture--create-id-for-entry creates and removes advice"
    (cl-letf* ((entry-id nil)
               ((symbol-function 'org-entry-put)
                (lambda (pom prop val)
                  (when (string= prop "ID")
                    (setq entry-id val))))
               ((symbol-function 'org-roam-capture--get)
                (lambda (prop)
                  (if (eq prop :id)
                      "test-id-123"
                    nil))))

      ;; Add the advice
      (advice-add #'org-capture-place-entry :after #'org-roam-capture--create-id-for-entry)

      ;; Call the function
      (org-roam-capture--create-id-for-entry)

      ;; Verify ID was set and advice removed
      (expect entry-id :to-equal "test-id-123")
      (expect (advice-member-p #'org-roam-capture--create-id-for-entry
                              #'org-capture-place-entry)
              :to-be nil)))

  (it "org-roam-capture--set-target-entry-p-a sets and removes advice"
    (cl-letf* ((captured-value nil)
               ((symbol-function 'org-capture-put)
                (lambda (prop val)
                  (when (eq prop :target-entry-p)
                    (setq captured-value val))))
               ((symbol-function 'org-roam-capture--get)
                (lambda (prop)
                  (if (eq prop :target-entry-p)
                      t
                    nil))))

      ;; Add the advice
      (advice-add #'org-capture-place-template :before #'org-roam-capture--set-target-entry-p-a)

      ;; Call the function
      (org-roam-capture--set-target-entry-p-a nil)

      ;; Verify value was set and advice removed
      (expect captured-value :to-be t)
      (expect (advice-member-p #'org-roam-capture--set-target-entry-p-a
                              #'org-capture-place-template)
              :to-be nil)))

  (it "org-roam-capture-run-new-node-hook-a runs hook when new node"
    (let ((hook-ran nil))
      (cl-letf* (((symbol-function 'org-roam-capture--get)
                  (lambda (prop)
                    (if (eq prop :new-node-p)
                        t
                      nil))))

        ;; Add test hook
        (add-hook 'org-roam-capture-new-node-hook
                  (lambda () (setq hook-ran t)))

        ;; Add the advice
        (advice-add #'org-capture-place-template :after #'org-roam-capture-run-new-node-hook-a)

        ;; Call the function
        (org-roam-capture-run-new-node-hook-a nil)

        ;; Verify hook ran
        (expect hook-ran :to-be t)

        ;; Clean up
        (remove-hook 'org-roam-capture-new-node-hook
                     (lambda () (setq hook-ran t)))))))

(describe "org-roam-capture target-entry-p detection"
  (it "detects entry target for file+olp"
    (let* ((temp-dir (make-temp-file "org-roam-test" t))
           (test-file (expand-file-name "test-olp.org" temp-dir))
           (org-roam-directory temp-dir)
           (org-roam-capture--node (org-roam-node-create :id (org-id-new)))
           (org-roam-capture--info (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Level 1\n** Level 2\n"))

            (cl-letf* (((symbol-function 'org-roam-capture--get-target)
                        (lambda () `(file+olp ,test-file ("Level 1" "Level 2"))))
                       ((symbol-function 'org-capture-get)
                        (lambda (prop) nil)))

              (with-current-buffer (find-file-noselect test-file)
                (org-roam-capture--setup-target-location)
                (expect (org-roam-capture--get :target-entry-p) :to-be t))))
        (delete-directory temp-dir t))))

  (it "detects non-entry target for file"
    (let* ((temp-dir (make-temp-file "org-roam-test" t))
           (test-file (expand-file-name "test-file.org" temp-dir))
           (org-roam-directory temp-dir)
           (org-roam-capture--node (org-roam-node-create :id (org-id-new)))
           (org-roam-capture--info (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "#+TITLE: Test\n"))

            (cl-letf* (((symbol-function 'org-roam-capture--get-target)
                        (lambda () `(file ,test-file)))
                       ((symbol-function 'org-capture-get)
                        (lambda (prop) nil)))

              (with-current-buffer (find-file-noselect test-file)
                (org-roam-capture--setup-target-location)
                (expect (org-roam-capture--get :target-entry-p) :to-be nil))))
        (delete-directory temp-dir t)))))

(provide 'test-org-roam-capture)

;;; test-org-roam-capture.el ends here
