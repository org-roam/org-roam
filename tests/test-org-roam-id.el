;;; test-org-roam-id.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-id-at-point"
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

  (it "returns the correct node ids"
    (find-file "tests/roam-files/family.org" nil)
    (expect (org-roam-id-at-point) :to-equal "998b2341-b7fe-434d-848c-5282c0727870")

    (search-forward "Grand")
    (expect (org-roam-id-at-point) :to-equal "77a90980-1994-464e-901f-7e3d3df07fd3")

    (search-forward "Child")
    (expect (org-roam-id-at-point) :to-equal "5fb4fdc5-b6d2-4f75-8d54-e60053e467ec")))

(describe "org-roam-id-find"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil)
    (org-roam-db-sync))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "finds nothing for non-existing node"
    (expect (org-roam-id-find "non-existing") :to-equal nil))

  (it "finds the correct file node"
    (let ((location (org-roam-id-find "884b2341-b7fe-434d-848c-5282c0727861")))
      (expect (car location) :to-match ".*/tests/roam-files/foo.org")
      (expect (cdr location) :to-equal 1)))

  (it "finds the correct heading node"
    (let ((location (org-roam-id-find "0fa5bb3e-3d8c-4966-8bc9-78d32e505d69")))
      (expect (car location) :to-match ".*/tests/roam-files/family.org")
      (expect (cdr location) :to-equal 156))))

(provide 'test-org-roam-id)

;;; test-org-roam-id.el ends here
