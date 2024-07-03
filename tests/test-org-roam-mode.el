;;; test-org-roam-mode.el --- Tests for Org-roam -*- lexical-binding: t; -*-

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

(describe "org-roam-unlinked-references--rg-command"
  (before-each
    ;; the space in the directory is on purpose
    (setq org-roam-directory "/tmp/org roam"))

  (it "returns the correct rg command for unlinked references"
    (expect (org-roam-unlinked-references--rg-command '("foo" "bar"))
            :to-equal
            "rg --follow --only-matching --vimgrep --pcre2 --ignore-case --glob \"*.org\" --glob \"*.org.gpg\" --glob \"*.org.age\" '\\[([^[]]++|(?R))*\\]|(\\bfoo\\b)|(\\bbar\\b)' /tmp/org\\ roam")))

(provide 'test-org-roam-mode)

;;; test-org-roam-mode.el ends here
