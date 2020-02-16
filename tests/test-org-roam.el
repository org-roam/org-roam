;;; test-org-roam.el --- Tests for org-roam -*- lexical-binding: t; -*-

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

;;

;;; Code:

;;;; Requirements

(require 'buttercup)
(require 'org-roam)

(defun abs-path (file-path)
  (file-truename (expand-file-name file-path org-roam-directory)))

;;; Tests
(describe "org-roam"
          (before-all
           (setq org-roam-directory (file-truename (concat default-directory "tests/roam-files"))))

          (it "org-roam--build-cache-async"
              (expect org-roam-cache-initialized :to-be nil)
              (expect (hash-table-count org-roam-forward-links-cache) :to-be 0)
              (expect (hash-table-count org-roam-backward-links-cache) :to-be 0)
              (expect (hash-table-count org-roam-titles-cache) :to-be 0)

              (org-roam--build-cache-async)
              (sleep-for 3) ;; Because it's async

              ;; Caches should be populated
              (expect org-roam-cache-initialized :to-be t)
              (expect (hash-table-count org-roam-forward-links-cache) :to-be 3)
              (expect (hash-table-count org-roam-backward-links-cache) :to-be 4)
              (expect (hash-table-count org-roam-titles-cache) :to-be 4)

              ;; Forward cache
              (let ((f1 (gethash (abs-path "f1.org") org-roam-forward-links-cache))
                    (f2 (gethash (abs-path "f2.org") org-roam-forward-links-cache))
                    (nested-f1 (gethash (abs-path "nested/f1.org") org-roam-forward-links-cache))
                    (nested-f2 (gethash (abs-path "nested/f2.org") org-roam-forward-links-cache))
                    (expected-f1 (list (abs-path "nested/f1.org")
                                       (abs-path "f2.org")))
                    (expected-nested-f1 (list (abs-path "nested/f2.org")
                                              (abs-path "f1.org")))
                    (expected-nested-f2 (list (abs-path "nested/f1.org"))))

                (expect f1 :to-have-same-items-as expected-f1)
                (expect f2 :to-be nil)
                (expect nested-f1 :to-have-same-items-as expected-nested-f1)
                (expect nested-f2 :to-have-same-items-as expected-nested-f2))

              ;; Backward cache
              (let ((f1 (hash-table-keys (gethash (abs-path "f1.org") org-roam-backward-links-cache)))
                    (f2 (hash-table-keys (gethash (abs-path "f2.org") org-roam-backward-links-cache)))
                    (nested-f1 (hash-table-keys(gethash (abs-path "nested/f1.org") org-roam-backward-links-cache)))
                    (nested-f2 (hash-table-keys (gethash (abs-path "nested/f2.org") org-roam-backward-links-cache)))
                    (expected-f1 (list (abs-path "nested/f1.org")))
                    (expected-f2 (list (abs-path "f1.org")))
                    (expected-nested-f1 (list (abs-path "nested/f2.org")
                                              (abs-path "f1.org")))
                    (expected-nested-f2 (list (abs-path "nested/f1.org"))))
                (expect f1 :to-have-same-items-as expected-f1)
                (expect f2 :to-have-same-items-as expected-f2)
                (expect nested-f1 :to-have-same-items-as expected-nested-f1)
                (expect nested-f2 :to-have-same-items-as expected-nested-f2))

              ;; Titles Cache
              (expect (gethash (abs-path "f1.org") org-roam-titles-cache) :to-equal "File 1")
              (expect (gethash (abs-path "f2.org") org-roam-titles-cache) :to-equal "File 2")
              (expect (gethash (abs-path "nested/f1.org") org-roam-titles-cache) :to-equal "Nested File 1")
              (expect (gethash (abs-path "nested/f2.org") org-roam-titles-cache) :to-equal "Nested File 2")
              (expect (gethash (abs-path "no-title.org") org-roam-titles-cache) :to-be nil)))
