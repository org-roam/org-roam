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
(require 'dash)

(defvar test-org-roam-directory (expand-file-name "tests/roam-files")
  "Directory containing org-roam test org files.")

(defun test-org-roam--init ()
  "."
  (let ((original-dir test-org-roam-directory)
        (new-dir (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-setup)))

(defun test-org-roam--teardown ()
  "."
  (org-roam-teardown)
  (delete-file org-roam-db-location)
  (org-roam-db--close))

(describe "test files for org-roam-db-sync"
  (before-all
    (test-org-roam--init))

  (after-all
    (test-org-roam--teardown))

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
