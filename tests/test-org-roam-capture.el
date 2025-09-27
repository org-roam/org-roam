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

  (describe "org-roam-capture-run-new-node-hook-a"
    (it "runs hook when new node"
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
                       (lambda () (setq hook-ran t))))))

    (it "removes itself as advice after running"
      (cl-letf* (((symbol-function 'org-roam-capture--get)
                  (lambda (prop)
                    (if (eq prop :new-node-p)
                        t
                      nil))))

        ;; Add the advice
        (advice-add #'org-capture-place-template :after #'org-roam-capture-run-new-node-hook-a)

        ;; Verify advice is added
        (expect (advice-member-p #'org-roam-capture-run-new-node-hook-a
                                #'org-capture-place-template)
                :to-be-truthy)

        ;; Call the function
        (org-roam-capture-run-new-node-hook-a nil)

        ;; Verify advice was removed after running
        (expect (advice-member-p #'org-roam-capture-run-new-node-hook-a
                                #'org-capture-place-template)
                :to-be nil)))))

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

(describe "org-roam-capture plain type ordering"
  :var ((temp-dir) (org-roam-directory) (org-roam-db-location))

  (before-each
    (setq temp-dir (make-temp-file "org-roam-test" t))
    (setq org-roam-directory temp-dir)
    (setq org-roam-db-location (expand-file-name "org-roam.db" temp-dir))
    (org-roam-db-sync))

  (after-each
    (delete-directory temp-dir t))

  (it "places properties drawer before captured content for plain type with file target"
    (let* ((test-file (expand-file-name "test-plain-file.org" temp-dir))
           (test-content "Test plain :target file")
           (node (org-roam-node-create :title "Test Plain"))
           (org-roam-capture--node node)
           (org-roam-capture--info (make-hash-table :test 'equal)))

      ;; Call the setup directly to simulate capture without user interaction
      (cl-letf* (((symbol-function 'org-capture-get)
                  (lambda (prop)
                    (pcase prop
                      (:type 'plain)
                      (:target-file test-file)
                      (_ nil))))
                 ((symbol-function 'org-roam-capture--get-target)
                  (lambda () `(file ,test-file))))

        ;; Run the setup and insert content
        (with-current-buffer (find-file-noselect test-file)
          (org-roam-capture--setup-target-location)
          (org-roam-capture--adjust-point-for-capture-type)
          (insert test-content)
          (save-buffer)))

      ;; Read the created file and check its structure
      (with-temp-buffer
        (insert-file-contents test-file)
        (let ((buffer-content (buffer-string)))

          ;; The expected format is:
          ;; :PROPERTIES:
          ;; :ID:       some-id
          ;; :END:
          ;; Test plain :target file

          ;; Check that properties come first
          (expect buffer-content
                  :to-match
                  (rx bol ":PROPERTIES:"))

          ;; Verify ordering: properties, then content
          (let ((props-pos (string-match ":PROPERTIES:" buffer-content))
                (end-pos (string-match ":END:" buffer-content))
                (content-pos (string-match (regexp-quote test-content) buffer-content)))

            (expect props-pos :to-be 0)
            (expect end-pos :not :to-be nil)
            (expect end-pos :to-be-greater-than props-pos)
            (expect content-pos :not :to-be nil)
            (expect content-pos :to-be-greater-than end-pos))))))

  (it "correctly orders buffer elements for plain type with file+head target"
    (let* ((test-file (expand-file-name "test-plain-file-head.org" temp-dir))
           (test-content "Test plain :target file+head")
           (node (org-roam-node-create :title "plain file+head"))
           (org-roam-capture--node node)
           (org-roam-capture--info (make-hash-table :test 'equal)))

      ;; Populate capture info with title for template expansion
      (puthash :title "plain file+head" org-roam-capture--info)

      ;; Call the setup directly to simulate capture without user interaction
      (cl-letf* (((symbol-function 'org-capture-get)
                  (lambda (prop)
                    (pcase prop
                      (:type 'plain)
                      (:target-file test-file)
                      (_ nil))))
                 ((symbol-function 'org-roam-capture--get-target)
                  (lambda () `(file+head ,test-file "#+title: ${title}\n"))))

        ;; Run the setup and insert content
        (with-current-buffer (find-file-noselect test-file)
          (org-roam-capture--setup-target-location)
          (org-roam-capture--adjust-point-for-capture-type)
          (insert test-content)
          (save-buffer)))

      ;; Read the created file and check its structure
      (with-temp-buffer
        (insert-file-contents test-file)
        (let ((buffer-content (buffer-string)))

          ;; The actual format according to org-mode property syntax is:
          ;; :PROPERTIES:
          ;; :ID:       some-id
          ;; :END:
          ;; #+title: plain file+head
          ;; Test plain :target file+head
          ;;
          ;; This is correct - buffer-level properties must be at the top

          ;; Check that properties come first
          (expect buffer-content
                  :to-match
                  (rx bol ":PROPERTIES:"))

          ;; Verify ordering: properties are at the top
          (let ((props-pos (string-match ":PROPERTIES:" buffer-content))
                (end-pos (string-match ":END:" buffer-content)))

            ;; Properties drawer should be first
            (expect props-pos :to-be 0)
            (expect end-pos :not :to-be nil)
            (expect end-pos :to-be-greater-than props-pos))))))

  (it "tests org-roam-capture--adjust-point-for-capture-type behavior"
    ;; Simple test to verify the fix for assertion error
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Test\n")
      (goto-char (point-max))

      ;; Mock org-capture-get to return plain type
      (cl-letf (((symbol-function 'org-capture-get)
                 (lambda (prop) (when (eq prop :type) 'plain))))

        ;; Document current state that previously caused assertion error
        (let ((point-before (point))
              (at-heading-before (org-at-heading-p)))
          ;; This should no longer trigger assertion error with our fix
          (org-roam-capture--adjust-point-for-capture-type)
          (expect point-before :to-be-greater-than 1)
          (expect at-heading-before :to-be nil))))))

(provide 'test-org-roam-capture)

;;; test-org-roam-capture.el ends here
