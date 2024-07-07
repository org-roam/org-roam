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

(provide 'test-org-roam-node)

;;; test-org-roam-node.el ends here
