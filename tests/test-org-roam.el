;;; test-org-roam.el --- Tests for org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup) (with-simulated-input))

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

;;

;;; Code:

;;;; Requirements

(require 'buttercup)
(require 'with-simulated-input)
(require 'org-roam)
(require 'dash)

(defun org-roam-test-abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (file-truename (expand-file-name file-path org-roam-directory)))

(defun org-roam-test-find-new-file (path)
  "PATH."
  (let ((path (org-roam-test-abs-path path)))
    (make-directory (file-name-directory path) t)
    (find-file path)))

(defvar org-roam-test-directory (file-truename (concat default-directory "tests/roam-files"))
  "Directory containing org-roam test org files.")

(defun org-roam-test-init ()
  "."
  (org-roam-db--close)
  (let ((original-dir org-roam-test-directory)
        (new-dir (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-mode +1)))

;;; Tests
(describe "org-roam-db-build-cache"
  (it "initializes correctly"
    (org-roam-test-init)
    (org-roam-db-build-cache)

    ;; Cache
    (expect (caar (org-roam-db-query [:select (funcall count) :from files])) :to-be 8)
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links])) :to-be 5)
    (expect (caar (org-roam-db-query [:select (funcall count) :from titles])) :to-be 8)
    (expect (caar (org-roam-db-query [:select (funcall count) :from titles
                                              :where titles :is-null])) :to-be 2)
    (expect (caar (org-roam-db-query [:select (funcall count) :from refs])) :to-be 1)

    ;; TODO Test files

    ;; Links -- File-from
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "nested/bar.org"))) :to-be 2)

    ;; Links -- File-to
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links
                                              :where (= file-to $s1)]
                                     (org-roam-test-abs-path "nested/foo.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links
                                              :where (= file-to $s1)]
                                     (org-roam-test-abs-path "nested/bar.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from file-links
                                              :where (= file-to $s1)]
                                     (org-roam-test-abs-path "unlinked.org"))) :to-be 0)
    ;; TODO Test titles
    (expect (org-roam-db-query [:select * :from titles])
            :to-have-same-items-as
            (list (list (org-roam-test-abs-path "alias.org")
                        (list "t1" "a1" "a 2"))
                  (list (org-roam-test-abs-path "bar.org")
                        (list "Bar"))
                  (list (org-roam-test-abs-path "foo.org")
                        (list "Foo"))
                  (list (org-roam-test-abs-path "nested/bar.org")
                        (list "Nested Bar"))
                  (list (org-roam-test-abs-path "nested/foo.org")
                        (list "Nested Foo"))
                  (list (org-roam-test-abs-path "no-title.org") nil)
                  (list (org-roam-test-abs-path "web_ref.org") nil)
                  (list (org-roam-test-abs-path "unlinked.org")
                        (list "Unlinked"))))

    (expect (org-roam-db-query [:select * :from refs])
            :to-have-same-items-as
            (list (list "https://google.com/" (org-roam-test-abs-path "web_ref.org"))))

    ;; Expect rebuilds to be really quick (nothing changed)
    (expect (org-roam-db-build-cache)
            :to-equal
            (list :files 0 :links 0 :titles 0 :refs 0 :deleted 0))))

(describe "org-roam-insert"
  (before-each
    (org-roam-test-init)
    (org-roam-db--clear)
    (org-roam-db-build-cache))

  (it "temp1 -> foo"
    (let ((buf (org-roam-test-find-new-file "temp1.org")))
      (with-current-buffer buf
        (with-simulated-input
            "Foo RET"
          (org-roam-insert nil))))
    (expect (buffer-string) :to-match (regexp-quote "file:foo.org")))

  (it "temp2 -> nested/foo"
    (let ((buf (org-roam-test-find-new-file "temp2.org")))
      (with-current-buffer buf
        (with-simulated-input
            "Nested SPC Foo RET"
          (org-roam-insert nil))))
    (expect (buffer-string) :to-match (regexp-quote "file:nested/foo.org")))

  (it "nested/temp3 -> foo"
    (let ((buf (org-roam-test-find-new-file "nested/temp3.org")))
      (with-current-buffer buf
        (with-simulated-input
            "Foo RET"
          (org-roam-insert nil))))
    (expect (buffer-string) :to-match (regexp-quote "file:../foo.org")))

  (it "a/b/temp4 -> nested/foo"
    (let ((buf (org-roam-test-find-new-file "a/b/temp4.org")))
      (with-current-buffer buf
        (with-simulated-input
            "Nested SPC Foo RET"
          (org-roam-insert nil))))
    (expect (buffer-string) :to-match (regexp-quote "file:../../nested/foo.org"))))

(describe "rename file updates cache"
  (before-each
    (org-roam-test-init)
    (org-roam-db--clear)
    (org-roam-db-build-cache))

  (it "foo -> new_foo"
    (rename-file (org-roam-test-abs-path "foo.org")
                 (org-roam-test-abs-path "new_foo.org"))
    ;; Cache should be cleared of old file
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from titles
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from refs
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)

    ;; Cache should be updated
    (expect (org-roam-db-query [:select [file-to]
                                        :from file-links
                                        :where (= file-from $s1)]
                               (org-roam-test-abs-path "new_foo.org"))
            :to-have-same-items-as
            (list (list (org-roam-test-abs-path "bar.org"))))
    (expect (org-roam-db-query [:select [file-from]
                                        :from file-links
                                        :where (= file-to $s1)]
                               (org-roam-test-abs-path "new_foo.org"))
            :to-have-same-items-as
            (list (list (org-roam-test-abs-path "nested/bar.org"))))

    ;; Links are updated
    (expect (with-temp-buffer
              (insert-file-contents (org-roam-test-abs-path "nested/bar.org"))
              (buffer-string))
            :to-match
            (regexp-quote "[[file:../new_foo.org][Foo]]")))

  (it "foo -> foo with spaces"
    (rename-file (org-roam-test-abs-path "foo.org")
                 (org-roam-test-abs-path "foo with spaces.org"))
    ;; Cache should be cleared of old file
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from titles
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from refs
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)

    ;; Cache should be updated
    (expect (org-roam-db-query [:select [file-to]
                                        :from file-links
                                        :where (= file-from $s1)]
                               (org-roam-test-abs-path "foo with spaces.org"))
            :to-have-same-items-as
            (list (list (org-roam-test-abs-path "bar.org"))))
    (expect (org-roam-db-query [:select [file-from]
                                        :from file-links
                                        :where (= file-to $s1)]
                               (org-roam-test-abs-path "foo with spaces.org"))
            :to-have-same-items-as
            (list (list (org-roam-test-abs-path "nested/bar.org"))))

    ;; Links are updated
    (expect (with-temp-buffer
              (insert-file-contents (org-roam-test-abs-path "nested/bar.org"))
              (buffer-string))
            :to-match
            (regexp-quote "[[file:../foo with spaces.org][Foo]]")))

  (it "no-title -> meaningful-title"
    (rename-file (org-roam-test-abs-path "no-title.org")
                 (org-roam-test-abs-path "meaningful-title.org"))
    ;; File has no forward links
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "no-title.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "meaningful-title.org"))) :to-be 1)

    ;; Links are updated with the appropriate name
    (expect (with-temp-buffer
              (insert-file-contents (org-roam-test-abs-path "meaningful-title.org"))
              (buffer-string))
            :to-match
            (regexp-quote "[[file:meaningful-title.org][meaningful-title]]")))

  (it "web_ref -> hello"
    (expect (org-roam-db-query
             [:select [file] :from refs
                      :where (= ref $s1)]
             "https://google.com/")
            :to-equal
            (list (list (org-roam-test-abs-path "web_ref.org"))))
    (rename-file (org-roam-test-abs-path "web_ref.org")
                 (org-roam-test-abs-path "hello.org"))
    (expect (org-roam-db-query
             [:select [file] :from refs
                      :where (= ref $s1)]
             "https://google.com/")
            :to-equal (list (list (org-roam-test-abs-path "hello.org"))))
    (expect (caar (org-roam-db-query
                   [:select [ref] :from refs
                            :where (= file $s1)]
                   (org-roam-test-abs-path "web_ref.org")))
            :to-equal nil)))

(describe "delete file updates cache"
  (before-each
    (org-roam-test-init)
    (org-roam-db--clear)
    (org-roam-db-build-cache)
    (sleep-for 1))

  (it "delete foo"
    (delete-file (org-roam-test-abs-path "foo.org"))
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from titles
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from refs
                                              :where (= file $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0)
    (expect (caar (org-roam-db-query [:select (funcall count)
                                              :from file-links
                                              :where (= file-from $s1)]
                                     (org-roam-test-abs-path "foo.org"))) :to-be 0))

  (it "delete web_ref"
    (expect (org-roam-db-query [:select * :from refs])
            :to-have-same-items-as
            (list (list "https://google.com/" (org-roam-test-abs-path "web_ref.org"))))
    (delete-file (org-roam-test-abs-path "web_ref.org"))
    (expect (org-roam-db-query [:select * :from refs])
            :to-have-same-items-as
            (list))))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
