;;; test-org-roam-link-replace.el --- Tests for Org-roam link replacement -*- lexical-binding: t; -*-

;; Copyright (C) 2025

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
;; Tests for commits fc86387 and 89dfaef - link replacement optimization
;;; Code:

(require 'buttercup)
(require 'org-roam)
(require 'org-roam-node)

(describe "org-roam-link-replace-all optimization"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory))
    (org-roam-db-sync))
  
  (after-all
    (org-roam-db--close)
    (delete-file org-roam-db-location))

  (it "only processes roam: links, not other bracket links"
    (with-temp-buffer
      (org-mode)
      (insert "[[file:test.org][File]]\n[[roam:Foo]]\n[[https://example.com][Web]]")
      (let ((replace-count 0)
            (original-fn (symbol-function 'org-roam-link-replace-at-point)))
        ;; Wrap the original function to count calls
        (cl-letf (((symbol-function 'org-roam-link-replace-at-point)
                   (lambda ()
                     (cl-incf replace-count)
                     (funcall original-fn))))
          (org-roam-link-replace-all)
          ;; Should only be called once, for the roam: link
          (expect replace-count :to-equal 1)
          (expect (buffer-string) :to-match "\\[\\[id:.*\\]\\[Foo\\]\\]"))))))

(provide 'test-org-roam-link-replace)

;;; test-org-roam-link-replace.el ends here
