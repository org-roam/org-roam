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

(describe "org-roam-db-sync"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory))
    (org-roam-setup))

  (after-all
    (org-roam-teardown)
    (delete-file org-roam-db-location))

  (it "has the correct number of files"
    (expect (caar (org-roam-db-query [:select (funcall count) :from files]))
            :to-equal
            2))

  (it "has the correct number of nodes"
    (expect (caar (org-roam-db-query [:select (funcall count) :from nodes]))
            :to-equal
            2))

  (it "has the correct number of links"
    (expect (caar (org-roam-db-query [:select (funcall count) :from links]))
            :to-equal
            1)))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
