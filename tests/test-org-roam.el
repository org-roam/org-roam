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

(defvar root-directory default-directory)

(describe "org-roam-file-p"
  (it "checks if given file respects criteria"
    (expect (org-roam-file-p "tests/roam-files/family.org") :to-equal nil)

    (setq org-roam-directory (expand-file-name "tests/roam-files"))
    (expect (org-roam-file-p "tests/roam-files/family.org") :to-equal t)
    (expect (org-roam-file-p "tests/roam-files/markdown.md") :to-equal nil)

    (setq org-roam-file-exclude-regexp (regexp-quote "family.org"))
    (expect (org-roam-file-p "tests/roam-files/family.org") :to-equal nil)))

(describe "org-roam-buffer-p"
  (it "checks if current buffer respects criteria"
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-file-exclude-regexp nil)
    (find-file "tests/roam-files/family.org" nil)
    (expect (org-roam-buffer-p) :to-equal t)
    (cd root-directory)))

(describe "org-roam-list-files"
  (before-each
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
          org-roam-file-extensions '("org")
          org-roam-file-exclude-regexp nil))

  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "gets files correctly"
    (expect (length (org-roam-list-files)) :to-equal 9))

  (it "respects org-roam-file-extensions"
    (setq org-roam-file-extensions '("md"))
    (expect (length (org-roam-list-files)) :to-equal 1)
    (setq org-roam-file-extensions '("org" "md"))
    (expect (length (org-roam-list-files)) :to-equal 10))

  (it "respects org-roam-file-exclude-regexp"
    (setq org-roam-file-exclude-regexp (regexp-quote "foo.org"))
    (expect (length (org-roam-list-files)) :to-equal 8)))

(describe "org-roam--list-files-search-globs"

  (it "returns the correct list of globs"
    (expect (org-roam--list-files-search-globs org-roam-file-extensions)
            :to-have-same-items-as
            '("\"*.org\"" "\"*.org.gpg\"" "\"*.org.age\""))))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
