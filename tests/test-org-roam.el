;;; test-org-roam.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-list-files"
  (before-each
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil))

  (it "gets files correctly"
    (expect (length (org-roam-list-files))
            :to-equal 4))

  (it "respects org-roam-file-extensions"
    (setq org-roam-file-extensions '("md"))
    (expect (length (org-roam-list-files)) :to-equal 1)
    (setq org-roam-file-extensions '("org" "md"))
    (expect (length (org-roam-list-files)) :to-equal 5))

  (it "respects org-roam-file-exclude-regexp"
    (setq org-roam-file-exclude-regexp (regexp-quote "foo.org"))
    (expect (length (org-roam-list-files)) :to-equal 3)))

(describe "org-roam-db-sync"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "has the correct number of files"
    (expect (caar (org-roam-db-query [:select (funcall count) :from files]))
            :to-equal
            4))

  (it "has the correct number of nodes"
    (expect (caar (org-roam-db-query [:select (funcall count) :from nodes]))
            :to-equal
            3))

  (it "has the correct number of links"
    (expect (caar (org-roam-db-query [:select (funcall count) :from links]))
            :to-equal
            1))

  (it "respects ROAM_EXCLUDE"
    ;; The excluded node has ID "53fadc75-f48e-461e-be06-44a1e88b2abe"
    (expect (mapcar #'car (org-roam-db-query [:select id :from nodes]))
            :to-have-same-items-as
            '("884b2341-b7fe-434d-848c-5282c0727861" "440795d0-70c1-4165-993d-aebd5eef7a24" "5b9a7400-f59c-4ef9-acbb-045b69af98f1")))

  (it "reads ref in quotes correctly"
    (expect (mapcar #'car (org-roam-db-query [:select [ref] :from refs]))
            :to-have-same-items-as
            '("//site.net/docs/01. introduction - hello world.html"))))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
