;;; test-org-roam-extract.el --- Tests for Org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jethro Kuan

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

(describe "org-roam--extract-node"
          (before-each
           (setq org-roam-directory (expand-file-name "tests/roam-files")
                 org-roam-db-location (expand-file-name "test-org-roam.db" temporary-file-directory)
                 org-roam-file-extensions '("org")
                 org-roam-file-exclude-regexp nil
                 org-id-track-globally nil
                 make-backup-files nil
                 template-file (expand-file-name "extract-test.txt" org-roam-directory)
                 source-file (expand-file-name "extract-test.org" org-roam-directory)
                 target-file (expand-file-name "extracted.org" org-roam-directory)
                 )

           (copy-file template-file source-file)
           (org-roam-db-sync)
           )
          (after-each
           (delete-file source-file)
           (delete-file target-file)
           (org-roam-db-sync)
           (delete-file org-roam-db-location)
           )

          (it "extracts an existing node"
              (expect (caar (org-roam-db-query [:select (funcall count) :from nodes :where (= id "unique-id-1")]))
                      :to-equal
                      1)
              (expect (caar (org-roam-db-query [:select (funcall count) :from nodes :where (= id "unique-id-2")]))
                      :to-equal
                      1)

              (with-current-buffer (find-file source-file)
                (expect
                 (org-roam-node-id (org-roam-node-at-point))
                 :to-equal "unique-id-1")
                (org-forward-heading-same-level 1)
                (expect
                 (org-roam-node-id (org-roam-node-at-point))
                 :to-equal "unique-id-2")
                (org-roam--extract-node target-file)
                (expect
                 (org-roam-node-id (org-roam-node-at-point))
                 :not :to-equal "unique-id-2")
                (kill-buffer)
                )
              (with-current-buffer (find-file target-file)
                (expect
                 (org-roam-node-id (org-roam-node-at-point))
                 :to-equal "unique-id-2")
                (kill-buffer)
                )
              )

          (it "converts a heading to a node before extracting"
              (with-current-buffer (find-file source-file)
                (org-goto-marker-or-bmk (org-find-exact-headline-in-buffer "Heading without ID"))
                (expect
                 (org-entry-get (point) "ID")
                 :to-be nil)
                (expect
                 (org-get-heading (point))
                 :to-equal "Heading without ID"
                 )
                (org-roam--extract-node target-file)
                (expect
                 (org-find-exact-headline-in-buffer "Heading without ID")
                 :to-be nil)
                (kill-buffer)
                )
              (with-current-buffer (find-file target-file)
                (expect
                 (org-roam-node-id (org-roam-node-at-point))
                 :not :to-be nil)
                (expect (org-roam-db--file-title)
                        :to-equal "Heading without ID")
                (kill-buffer)
                )
              )

          (it "calls optional hook function after extraction"
              (setf (symbol-function 'test-hook)
                    (lambda (node)
                      ;; no-op
                      nil))
              (spy-on 'test-hook)

              (add-hook 'org-roam-post-extraction-functions #'test-hook)

              (with-current-buffer (find-file source-file)
                (org-goto-marker-or-bmk (org-find-exact-headline-in-buffer "Node to be extracted"))
                (org-roam--extract-node target-file)
                (kill-buffer)
                )
              (with-current-buffer (find-file target-file)
                (expect 'test-hook :to-have-been-called-with (org-roam-node-at-point))
                (kill-buffer)
                )
              (remove-hook 'org-roam-post-extraction-functions #'test-hook)
              )
          )

(provide 'test-org-roam-extract)

;;; test-org-roam-extract.el ends here
