;;; test-org-roam.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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
;;; Code:

(require 'buttercup)
(require 'org-roam)
(require 'dash)

(defun test-org-roam--abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (file-truename (expand-file-name file-path org-roam-directory)))

(defun test-org-roam--find-file (path)
  "PATH."
  (let ((path (test-org-roam--abs-path path)))
    (make-directory (file-name-directory path) t)
    (find-file path)))

(defvar test-org-roam-directory (file-truename (concat default-directory "tests/roam-files"))
  "Directory containing org-roam test org files.")

(defun test-org-roam--init ()
  "."
  (let ((original-dir test-org-roam-directory)
        (new-dir (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-mode +1)
    (sleep-for 2)))

(defun test-org-roam--teardown ()
  (org-roam-mode -1)
  (delete-file (org-roam-db--get))
  (org-roam-db--close))

(describe "org-roam--str-to-list"
  (it "nil"
    (expect (org-roam--str-to-list nil)
            :to-be
            nil))
  (it "\"multi word\" prop 123"
    (expect (org-roam--str-to-list "\"multi word\" prop 123")
            :to-equal
            '("multi word" "prop" "123")))
  (it "prop \"multi word\" 123"
    (expect (org-roam--str-to-list "\"multi word\" prop 123")
            :to-equal
            '("multi word" "prop" "123")))
  (it "errors on bad input"
    (expect (org-roam--str-to-list 1)
            :to-throw)
    (expect (org-roam--str-to-list "\"hello")
            :to-throw)))

(describe "Title extraction"
  :var (org-roam-title-sources)
  (before-all
    (test-org-roam--init))

  (after-all
    (test-org-roam--teardown))

  (cl-flet
      ((test (fn file)
             (let ((buf (find-file-noselect
                         (test-org-roam--abs-path file))))
               (with-current-buffer buf
                 (funcall fn)))))
    (it "extracts title from title property"
      (expect (test #'org-roam--extract-titles-title
                    "titles/title.org")
              :to-equal
              '("Title"))
      (expect (test #'org-roam--extract-titles-title
                    "titles/aliases.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-title
                    "titles/headline.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-title
                    "titles/combination.org")
              :to-equal
              '("TITLE PROP")))

    (it "extracts alias"
      (expect (test #'org-roam--extract-titles-alias
                    "titles/title.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-alias
                    "titles/aliases.org")
              :to-equal
              '("roam" "alias"))
      (expect (test #'org-roam--extract-titles-alias
                    "titles/headline.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-alias
                    "titles/combination.org")
              :to-equal
              '("roam" "alias")))

    (it "extracts headlines"
      (expect (test #'org-roam--extract-titles-alias
                    "titles/title.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-headline
                    "titles/aliases.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-titles-headline
                    "titles/headline.org")
              :to-equal
              '("Headline"))
      (expect (test #'org-roam--extract-titles-headline
                    "titles/combination.org")
              :to-equal
              '("Headline")))

    (describe "uses org-roam-title-sources correctly"
      (it "'((title headline) alias)"
        (expect (let ((org-roam-title-sources '((title headline) alias)))
                  (test #'org-roam--extract-titles
                        "titles/combination.org"))
                :to-equal
                '("TITLE PROP" "roam" "alias")))
      (it "'((headline title) alias)"
        (expect (let ((org-roam-title-sources '((headline title) alias)))
                  (test #'org-roam--extract-titles
                        "titles/combination.org"))
                :to-equal
                '("Headline" "roam" "alias")))
      (it "'(headline alias title)"
        (expect (let ((org-roam-title-sources '(headline alias title)))
                  (test #'org-roam--extract-titles
                        "titles/combination.org"))
                :to-equal
                '("Headline" "roam" "alias" "TITLE PROP"))))))

(describe "Tag extraction"
  :var (org-roam-tag-sources)
  (before-all
    (test-org-roam--init))

  (after-all
    (test-org-roam--teardown))

  (cl-flet
      ((test (fn file)
             (let* ((fname (test-org-roam--abs-path file))
                    (buf (find-file-noselect fname)))
               (with-current-buffer buf
                 (funcall fn fname)))))
    (it "extracts from prop"
      (expect (test #'org-roam--extract-tags-prop
                    "tags/tag.org")
              :to-equal
              '("t1" "t2 with space" "t3"))
      (expect (test #'org-roam--extract-tags-prop
                    "tags/no_tag.org")
              :to-equal
              nil))

    (it "extracts from all directories"
      (expect (test #'org-roam--extract-tags-all-directories
                    "base.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-tags-all-directories
                    "tags/tag.org")
              :to-equal
              '("tags"))
      (expect (test #'org-roam--extract-tags-all-directories
                    "nested/deeply/deeply_nested_file.org")
              :to-equal
              '("nested" "deeply")))

    (it "extracts from last directory"
      (expect (test #'org-roam--extract-tags-last-directory
                    "base.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-tags-last-directory
                    "tags/tag.org")
              :to-equal
              '("tags"))
      (expect (test #'org-roam--extract-tags-last-directory
                    "nested/deeply/deeply_nested_file.org")
              :to-equal
              '("deeply")))

    (describe "uses org-roam-tag-sources correctly"
      (it "'(prop)"
        (expect (let ((org-roam-tag-sources '(prop)))
                  (test #'org-roam--extract-tags
                        "tags/tag.org"))
                :to-equal
                '("t1" "t2 with space" "t3")))
      (it "'(prop all-directories)"
        (expect (let ((org-roam-tag-sources '(prop all-directories)))
                  (test #'org-roam--extract-tags
                        "tags/tag.org"))
                :to-equal
                '("t1" "t2 with space" "t3" "tags"))))))

;;; Tests
(xdescribe "org-roam-db-build-cache"
  (before-each
    (test-org-roam--init))

  (after-each
    (test-org-roam--teardown))

  (it "initializes correctly"
    ;; Cache
    (expect (caar (org-roam-db-query [:select (funcall count) :from files])) :to-be 8)
    (expect (caar (org-roam-db-query [:select (funcall count) :from links])) :to-be 5)
    (expect (caar (org-roam-db-query [:select (funcall count) :from titles])) :to-be 8)
    (expect (caar (org-roam-db-query [:select (funcall count) :from titles
                                      :where titles :is-null])) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from refs])) :to-be 1)

    ;; Links
    (expect (caar (org-roam-db-query [:select (funcall count) :from links
                                      :where (= from $s1)]
                                     (test-org-roam--abs-path "foo.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from links
                                      :where (= from $s1)]
                                     (test-org-roam--abs-path "nested/bar.org"))) :to-be 2)

    ;; Links -- File-to
    (expect (caar (org-roam-db-query [:select (funcall count) :from links
                                      :where (= to $s1)]
                                     (test-org-roam--abs-path "nested/foo.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from links
                                      :where (= to $s1)]
                                     (test-org-roam--abs-path "nested/bar.org"))) :to-be 1)
    (expect (caar (org-roam-db-query [:select (funcall count) :from links
                                      :where (= to $s1)]
                                     (test-org-roam--abs-path "unlinked.org"))) :to-be 0)
    ;; TODO Test titles
    (expect (org-roam-db-query [:select * :from titles])
            :to-have-same-items-as
            (list (list (test-org-roam--abs-path "alias.org")
                        (list "t1" "a1" "a 2"))
                  (list (test-org-roam--abs-path "bar.org")
                        (list "Bar"))
                  (list (test-org-roam--abs-path "foo.org")
                        (list "Foo"))
                  (list (test-org-roam--abs-path "nested/bar.org")
                        (list "Nested Bar"))
                  (list (test-org-roam--abs-path "nested/foo.org")
                        (list "Nested Foo"))
                  (list (test-org-roam--abs-path "no-title.org")
                        (list "Headline title"))
                  (list (test-org-roam--abs-path "web_ref.org") nil)
                  (list (test-org-roam--abs-path "unlinked.org")
                        (list "Unlinked"))))

    (expect (org-roam-db-query [:select * :from refs])
            :to-have-same-items-as
            (list (list "https://google.com/" (test-org-roam--abs-path "web_ref.org") "website")))

    ;; Expect rebuilds to be really quick (nothing changed)
    (expect (org-roam-db-build-cache)
            :to-equal
            (list :files 0 :links 0 :tags 0 :titles 0 :refs 0 :deleted 0))))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
