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
    (expect (org-roam-db-get-scheduled-time) :to-equal "2024-07-16T00:00:00+0000")))

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
    (expect (org-roam-db-get-deadline-time) :to-equal "2024-07-17T00:00:00+0000")))

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

  (it "has the correct number of files"
    (expect (caar (org-roam-db-query [:select (funcall count) :from files]))
            :to-equal
            9))

  (it "has the correct number of nodes"
    (expect (caar (org-roam-db-query [:select (funcall count) :from nodes]))
            :to-equal
            12))

  (it "has the correct number of links"
    (expect (caar (org-roam-db-query [:select (funcall count) :from links]))
            :to-equal
            1))

  (it "respects ROAM_EXCLUDE"
    ;; The excluded node has ID "53fadc75-f48e-461e-be06-44a1e88b2abe"
    (expect (mapcar #'car (org-roam-db-query [:select id :from nodes]))
            :to-have-same-items-as
            '("884b2341-b7fe-434d-848c-5282c0727861"
              "440795d0-70c1-4165-993d-aebd5eef7a24"
              "5b9a7400-f59c-4ef9-acbb-045b69af98f1"
              "0fa5bb3e-3d8c-4966-8bc9-78d32e505d69"
              "5fb4fdc5-b6d2-4f75-8d54-e60053e467ec"
              "77a90980-1994-464e-901f-7e3d3df07fd3"
              "57ff3ce7-5bda-4825-8fca-c09f523e87ba"
              "998b2341-b7fe-434d-848c-5282c0727870"
              "a523c198-4cb4-44d2-909c-a0e3258089cd"
              "3ab84701-d1c1-463f-b5c6-715e6ff5a0bf"
              "9a20ca6c-5555-41c9-a039-ac38bf59c7a9"
              "97bf31cf-dfee-45d8-87a5-2ae0dabc4734")))

  (it "reads ref in quotes correctly"
    (expect (mapcar #'car (org-roam-db-query [:select [ref] :from refs]))
            :to-have-same-items-as
            '("//site.net/docs/01. introduction - hello world.html"))))

(provide 'test-org-roam-db)

;;; test-org-roam-db.el ends here
