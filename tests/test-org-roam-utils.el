;;; test-org-roam-utils.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-whitespace-content"
  (it "extracts whitespace correctly"
    (expect
     (org-roam-whitespace-content "foo")
     :to-equal "")
    (expect
     (org-roam-whitespace-content "foo\n")
     :to-equal "\n")
    (expect
     (org-roam-whitespace-content "foo\n\t\n")
     :to-equal "\n\t\n")))

(describe "org-roam-db--file-title"
  (it "supports normal titles"
    (expect
     (with-temp-buffer
       (insert "#+title:normal title")
       (org-roam-db--file-title))
     :to-equal "normal title"))
  (it "supports multi-line titles"
    (expect
     (with-temp-buffer
       (insert "#+title: title:\n#+title: separated by newline")
       (org-roam-db--file-title))
     :to-equal "title: separated by newline"))
  (it "supports file-name based titles"
    (progn
      (setq org-roam-directory temporary-file-directory
            org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
            org-roam-file-extensions '("org"))
      (with-temp-buffer
        (write-file (expand-file-name "test file.org" org-roam-directory))
        (org-roam-db--file-title)))
    :to-equal "test file"))

(provide 'test-org-roam-utils)
