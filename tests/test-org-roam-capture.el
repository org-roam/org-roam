;;; test-org-roam-capture.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-capture--fill-template"
  (it "fills template without newline"
    (expect
     (org-roam-capture--fill-template "foo")
     :to-equal "foo"))

  (it "fills template ensuring newline"
    (expect
     (org-roam-capture--fill-template "foo" 'ensure-newline)
     :to-equal "foo\n"))

  (it "fills template with newline"
    (expect
     (org-roam-capture--fill-template "foo\n")
     :to-equal "foo\n"))

  (it "fills template with two newlines"
    (expect
     (org-roam-capture--fill-template "foo\n\n")
     :to-equal "foo\n\n")
    (expect
     (org-roam-capture--fill-template "foo\n\t\n")
     :to-equal "foo\n\t\n"))

  (it "fills template without deleting newlines in its body"
    (expect
     (org-roam-capture--fill-template "foo\n\n\nbar\n\n")
     :to-equal "foo\n\n\nbar\n\n"))

  (it "expands templates when it's a function"
    (expect
     (org-roam-capture--fill-template (lambda () "foo"))
     :to-equal "foo")))

(provide 'test-org-roam-capture)

;;; test-org-roam-capture.el ends here
