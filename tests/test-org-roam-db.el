;;; test-org-roam-db.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-db-get-scheduled-time"
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

  (it "should get scheduled time for current heading node"
    (org-roam-id-open "a523c198-4cb4-44d2-909c-a0e3258089cd" nil)
    (expect (org-roam-db-get-scheduled-time) :to-equal "2024-07-16T00:00:00")))

(describe "org-roam-db-get-deadline-time"
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

  (it "should get deadline time for current heading node"
    (org-roam-id-open "3ab84701-d1c1-463f-b5c6-715e6ff5a0bf" nil)
    (expect (org-roam-db-get-deadline-time) :to-equal "2024-07-17T00:00:00")))

(describe "org-roam-db--file-hash"
  (it "computes the SHA1 of file"
    (expect (org-roam-db--file-hash "tests/roam-files/family.org")
            :to-equal
            "c4ebf8918c1533df72e4d182cbf1bbd90f776b3b")))

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

  (it "makes the correct number of rows in files table"
    (expect (caar (org-roam-db-query [:select (funcall count) :from files]))
            :to-equal
            14))

  (it "makes the correct number of rows in nodes table"
    (expect (caar (org-roam-db-query [:select (funcall count) :from nodes]))
            :to-equal
            30))

  (it "makes the correct number of rows in links table"
    (expect (caar (org-roam-db-query [:select (funcall count) :from links]))
            :to-equal
            6))

  (it "respects ROAM_EXCLUDE"
    (expect (mapcar #'car (org-roam-db-query [:select id :from nodes]))
            :not :to-contain "53fadc75-f48e-461e-be06-44a1e88b2abe")
    (expect (mapcar #'car (org-roam-db-query [:select id :from nodes]))
            :not :to-contain "aa3b8409-e918-44af-8f2f-b4639f812573"))

  (it "reads ref in quotes correctly"
    (expect (mapcar #'car (org-roam-db-query [:select [ref] :from refs]))
            :to-have-same-items-as
            '("//site.net/docs/01. introduction - hello world.html"))))

(provide 'test-org-roam-db)

;;; test-org-roam-db.el ends here
