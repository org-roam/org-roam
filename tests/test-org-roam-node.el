;;; test-org-roam-node.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(defvar root-directory default-directory)

(describe "org-roam-node-from-id"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "returns nil for unknown id"
    (expect (org-roam-node-from-id "non-existing") :to-equal nil))

  (it "returns correct node from id"
    (let ((node (org-roam-node-from-id "884b2341-b7fe-434d-848c-5282c0727861")))
      (expect (org-roam-node-title node) :to-equal "Foo"))))

(describe "org-roam-node-from-title-or-alias"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "returns nil for unknown title"
    (expect (org-roam-node-from-title-or-alias "non-existing") :to-equal nil))

  (it "returns correct node from title"
    (let ((node (org-roam-node-from-title-or-alias "Foo")))
      (expect (org-roam-node-title node) :to-equal "Foo")))

  (it "returns correct node from alias"
    (let ((node (org-roam-node-from-title-or-alias "Batman")))
      (expect (org-roam-node-title node) :to-equal "Bruce Wayne")))

  (it "returns correct node from alias with nocase"
    (let ((node (org-roam-node-from-title-or-alias "batman" t)))
      (expect (org-roam-node-title node) :to-equal "Bruce Wayne"))))

(describe "org-roam-demote-entire-buffer"
  (after-each
    (cd root-directory))

  (it "demotes an entire org buffer"
    (find-file "tests/roam-files/demoteable.org" nil)
    (org-roam-demote-entire-buffer)
    (expect (buffer-substring-no-properties (point) (point-max))
            :to-equal "* Demoteable\n:PROPERTIES:\n:ID: 97bf31cf-dfee-45d8-87a5-2ae0dabc4734\n:END:\n\n** Demoteable h1\n\n*** Demoteable child\n")))

(describe "org-roam--h1-count"
  (after-each
    (cd root-directory))

  (it "returns the correct number of level-1 headings"
    (find-file "tests/roam-files/foo.org" nil)
    (expect (org-roam--h1-count) :to-equal 0)

    (cd root-directory)

    (find-file "tests/roam-files/family.org" nil)
    (expect (org-roam--h1-count) :to-equal 1)))

(describe "org-roam--buffer-promoteable-p"
  (after-each
    (cd root-directory))

  (it "should check if buffer is promoteable"
    (find-file "tests/roam-files/foo.org" nil)
    (expect (org-roam--buffer-promoteable-p) :to-equal nil)

    (cd root-directory)

    (find-file "tests/roam-files/family.org" nil)
    (expect (org-roam--buffer-promoteable-p) :to-equal nil)

    (cd root-directory)

    (find-file "tests/roam-files/promoteable.org" nil)
    (expect (org-roam--buffer-promoteable-p) :to-equal t)))

(describe "org-roam--get-titles"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "returns the list of titles and aliases"
    (expect (org-roam--get-titles)
            :to-have-same-items-as
            `("Bar" "Batman" "Bruce Wayne" "Child" "Deadline heading" "Demoteable" "Family"
              "Foo" "Grand-Parent" "Parent" "ref with space" "Scheduled heading" "With Times"))))


(describe "org-roam-alias"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location)
    (cd root-directory))

  (it "adds an alias to a node"
    (cd root-directory)
    (find-file "tests/roam-files/foo.org" nil)
    (org-roam-alias-add "qux")
    (expect (buffer-substring-no-properties (point) (point-max))
            :to-equal ":PROPERTIES:\n:ID:       884b2341-b7fe-434d-848c-5282c0727861\n:ROAM_ALIASES: qux\n:END:\n#+title: Foo\n"))

  (it "removes an alias from a node"
    (cd root-directory)
    (find-file "tests/roam-files/with-alias.org" nil)
    (org-roam-alias-remove "Batman")
    (expect (buffer-substring-no-properties (point) (point-max))
            :to-equal ":PROPERTIES:\n:ID: 57ff3ce7-5bda-4825-8fca-c09f523e87ba\n:END:\n#+title: Bruce Wayne\n")))

(provide 'test-org-roam-node)

;;; test-org-roam-node.el ends here
