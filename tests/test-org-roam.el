;;; test-org-roam.el --- Tests for org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup) (with-simulated-input))

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
(require 'with-simulated-input)
(require 'org-roam)
(require 'dash)

(defun abs-path (file-path)
  (file-truename (expand-file-name file-path org-roam-directory)))

(defun org-roam--test-find-new-file (path)
  (let ((path (abs-path path)))
    (make-directory (file-name-directory path) t)
    (find-file path)))

(defvar org-roam--tests-directory (file-truename (concat default-directory "tests/roam-files"))
  "Directory containing org-roam test org files.")

(defvar org-roam--tests-multi (file-truename (concat default-directory "tests/roam-files-multi"))
  "Directory containing org-roam test org files.")

(defun org-roam--test-init ()
  (let ((original-dir org-roam--tests-directory)
        (new-dir (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (setq org-roam-mute-cache-build t))
  (org-roam-mode +1))

(defun org-roam--test-multi-init ()
  (let ((original-dir-1 org-roam--tests-directory)
        (original-dir-2 org-roam--tests-multi)
        (new-dir-1 (expand-file-name (make-temp-name "org-roam") temporary-file-directory))
        (new-dir-2 (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir-1 new-dir-1)
    (copy-directory original-dir-2 new-dir-2)
    (setq org-roam-directory new-dir-1)
    (setq org-roam-directory2 new-dir-2)
    (setq org-roam-mute-cache-build t))
  (org-roam-mode +1))

(defun org-roam--test-build-cache ()
  "Builds the caches synchronously."
  (let ((cache (org-roam--build-cache org-roam-directory)))
    (org-roam--set-directory-cache
     (org-roam-cache :initialized t
                     :forward-links (plist-get cache :forward)
                     :backward-links (plist-get cache :backward)
                     :titles (plist-get cache :titles)))))

;;; Tests
(describe "org-roam--build-cache-async"
          (it "initializes correctly"
              (org-roam--clear-cache)
              (org-roam--test-multi-init)
              (expect (org-roam--cache-initialized-p) :to-be nil)
              (expect (hash-table-count (org-roam--forward-links-cache)) :to-be 0)
              (expect (hash-table-count (org-roam--backward-links-cache)) :to-be 0)
              (expect (hash-table-count (org-roam--titles-cache)) :to-be 0)

              (org-roam--build-cache-async)
              (sleep-for 3) ;; Because it's async

              ;; Caches should be populated
              (expect (org-roam--cache-initialized-p) :to-be t)
              (expect (hash-table-count (org-roam--forward-links-cache)) :to-be 4)
              (expect (hash-table-count (org-roam--backward-links-cache)) :to-be 5)
              (expect (hash-table-count (org-roam--titles-cache)) :to-be 6)

              ;; Forward cache
              (let ((f1 (gethash (abs-path "f1.org")
                                 (org-roam--forward-links-cache)))
                    (f2 (gethash (abs-path "f2.org")
                                 (org-roam--forward-links-cache)))
                    (nested-f1 (gethash (abs-path "nested/f1.org")
                                        (org-roam--forward-links-cache)))
                    (nested-f2 (gethash (abs-path "nested/f2.org")
                                        (org-roam--forward-links-cache)))
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
              (let ((f1 (hash-table-keys (gethash (abs-path "f1.org")
                                                  (org-roam--backward-links-cache))))
                    (f2 (hash-table-keys (gethash (abs-path "f2.org")
                                                  (org-roam--backward-links-cache))))
                    (nested-f1 (hash-table-keys
                                (gethash (abs-path "nested/f1.org")
                                         (org-roam--backward-links-cache))))
                    (nested-f2 (hash-table-keys
                                (gethash (abs-path "nested/f2.org")
                                         (org-roam--backward-links-cache))))
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
              (expect (gethash (abs-path "f1.org")
                               (org-roam--titles-cache)) :to-equal (list "File 1"))
              (expect (gethash (abs-path "f2.org")
                               (org-roam--titles-cache)) :to-equal (list "File 2"))
              (expect (gethash (abs-path "nested/f1.org")
                               (org-roam--titles-cache)) :to-equal (list "Nested File 1"))
              (expect (gethash (abs-path "nested/f2.org")
                               (org-roam--titles-cache)) :to-equal (list "Nested File 2"))
              (expect (gethash (abs-path "alias.org")
                               (org-roam--titles-cache)) :to-equal (list "t1" "a1" "a 2"))
              (expect (gethash (abs-path "no-title.org")
                               (org-roam--titles-cache)) :to-be nil)

              ;; Multi
              (let ((org-roam-directory org-roam-directory2))
                (org-roam--build-cache-async)
                (sleep-for 3) ;; Because it's async

                ;; Caches should be populated
                (expect (org-roam--cache-initialized-p) :to-be t)
                (expect (hash-table-count (org-roam--forward-links-cache)) :to-be 4)
                (expect (hash-table-count (org-roam--backward-links-cache)) :to-be 5)
                (expect (hash-table-count (org-roam--titles-cache)) :to-be 5)

                ;; Forward cache
                (let ((mf1 (gethash (abs-path "mf1.org")
                                    (org-roam--forward-links-cache)))
                      (mf2 (gethash (abs-path "mf2.org")
                                    (org-roam--forward-links-cache)))
                      (nested-mf1 (gethash (abs-path "nested/mf1.org")
                                           (org-roam--forward-links-cache)))
                      (nested-mf2 (gethash (abs-path "nested/mf2.org")
                                           (org-roam--forward-links-cache)))
                      (expected-mf1 (list (abs-path "nested/mf1.org")
                                          (abs-path "mf2.org")))
                      (expected-nested-mf1 (list (abs-path "nested/mf2.org")
                                                 (abs-path "mf1.org")))
                      (expected-nested-mf2 (list (abs-path "nested/mf1.org"))))

                  (expect mf1 :to-have-same-items-as expected-mf1)
                  (expect mf2 :to-be nil)
                  (expect nested-mf1 :to-have-same-items-as expected-nested-mf1)
                  (expect nested-mf2 :to-have-same-items-as expected-nested-mf2))

                ;; Backward cache
                (let ((mf1 (hash-table-keys
                            (gethash (abs-path "mf1.org")
                                     (org-roam--backward-links-cache))))
                      (mf2 (hash-table-keys
                            (gethash (abs-path "mf2.org")
                                     (org-roam--backward-links-cache))))
                      (nested-mf1 (hash-table-keys
                                   (gethash (abs-path "nested/mf1.org")
                                            (org-roam--backward-links-cache))))
                      (nested-mf2 (hash-table-keys
                                   (gethash (abs-path "nested/mf2.org")
                                            (org-roam--backward-links-cache))))
                      (expected-mf1 (list (abs-path "nested/mf1.org")))
                      (expected-mf2 (list (abs-path "mf1.org")))
                      (expected-nested-mf1 (list (abs-path "nested/mf2.org")
                                                 (abs-path "mf1.org")))
                      (expected-nested-mf2 (list (abs-path "nested/mf1.org"))))
                  (expect mf1 :to-have-same-items-as expected-mf1)
                  (expect mf2 :to-have-same-items-as expected-mf2)
                  (expect nested-mf1 :to-have-same-items-as expected-nested-mf1)
                  (expect nested-mf2 :to-have-same-items-as expected-nested-mf2))

                ;; Titles Cache
                (expect (gethash (abs-path "mf1.org")
                                 (org-roam--titles-cache))
                        :to-equal (list "Multi-File 1"))
                (expect (gethash (abs-path "mf2.org")
                                 (org-roam--titles-cache))
                        :to-equal (list "Multi-File 2"))
                (expect (gethash (abs-path "nested/mf1.org")
                                 (org-roam--titles-cache))
                        :to-equal (list "Nested Multi-File 1"))
                (expect (gethash (abs-path "nested/mf2.org")
                                 (org-roam--titles-cache))
                        :to-equal (list "Nested Multi-File 2"))
                (expect (gethash (abs-path "no-title.org")
                                 (org-roam--titles-cache))
                        :to-be nil))))

(describe "org-roam-insert"
          (before-each
           (org-roam--test-init)
           (org-roam--clear-cache)
           (org-roam--test-build-cache))

          (it "temp1 -> f1"
              (let ((buf (org-roam--test-find-new-file "temp1.org")))
                (with-current-buffer buf
                  (with-simulated-input
                   "File SPC 1 RET"
                   (org-roam-insert nil))))
              (expect (buffer-string) :to-match (regexp-quote "file:f1.org")))

          (it "temp2 -> nested/f1"
              (let ((buf (org-roam--test-find-new-file "temp2.org")))
                (with-current-buffer buf
                  (with-simulated-input
                   "Nested SPC File SPC 1 RET"
                   (org-roam-insert nil))))
              (expect (buffer-string) :to-match (regexp-quote "file:nested/f1.org")))

          (it "nested/temp3 -> f1"
              (let ((buf (org-roam--test-find-new-file "nested/temp3.org")))
                (with-current-buffer buf
                  (with-simulated-input
                   "File SPC 1 RET"
                   (org-roam-insert nil))))
              (expect (buffer-string) :to-match (regexp-quote "file:../f1.org")))

          (it "a/b/temp4 -> nested/f1"
              (let ((buf (org-roam--test-find-new-file "a/b/temp4.org")))
                (with-current-buffer buf
                  (with-simulated-input
                   "Nested SPC File SPC 1 RET"
                   (org-roam-insert nil))))
              (expect (buffer-string) :to-match (regexp-quote "file:../../nested/f1.org"))))

(describe "rename file updates cache"
          (before-each
           (org-roam--test-init)
           (org-roam--clear-cache)
           (org-roam--test-build-cache))

          (it "f1 -> new_f1"
              (rename-file (abs-path "f1.org")
                           (abs-path "new_f1.org"))
              ;; Cache should be cleared of old file
              (expect (gethash (abs-path "f1.org") (org-roam--forward-links-cache)) :to-be nil)
              (expect (->> (org-roam--backward-links-cache)
                           (gethash (abs-path "nested/f1.org"))
                           (hash-table-keys)
                           (member (abs-path "f1.org"))) :to-be nil)

              (expect (->> (org-roam--forward-links-cache)
                           (gethash (abs-path "new_f1.org"))) :not :to-be nil)

              (expect (->> (org-roam--forward-links-cache)
                           (gethash (abs-path "new_f1.org"))
                           (member (abs-path "nested/f1.org"))) :not :to-be nil)

              ;; Links are updated
              (expect (with-temp-buffer
                        (insert-file-contents (abs-path "nested/f1.org"))
                        (buffer-string)) :to-match (regexp-quote "[[file:../new_f1.org][File 1]]")))

          (it "f1 -> f1 with spaces"
              (rename-file (abs-path "f1.org")
                           (abs-path "f1 with spaces.org"))
              ;; Cache should be cleared of old file
              (expect (gethash (abs-path "f1.org")  (org-roam--forward-links-cache)) :to-be nil)
              (expect (->> (org-roam--backward-links-cache)
                           (gethash (abs-path "nested/f1.org"))
                           (hash-table-keys)
                           (member (abs-path "f1.org"))) :to-be nil)
              ;; Links are updated
              (expect (with-temp-buffer
                        (insert-file-contents (abs-path "nested/f1.org"))
                        (buffer-string)) :to-match (regexp-quote "[[file:../f1 with spaces.org][File 1]]")))

          (it "no-title -> meaningful-title"
              (rename-file (abs-path "no-title.org")
                           (abs-path "meaningful-title.org"))
              ;; File has no forward links
              (expect (gethash (abs-path "no-title.org")  (org-roam--forward-links-cache)) :to-be nil)
              (expect (gethash (abs-path "meaningful-title.org")
                               (org-roam--forward-links-cache)) :to-be nil)

              (expect (->> (org-roam--forward-links-cache)
                           (gethash (abs-path "f3.org"))
                           (member (abs-path "no-title.org"))) :to-be nil)

              (expect (->> (org-roam--forward-links-cache)
                           (gethash (abs-path "f3.org"))
                           (member (abs-path "meaningful-title.org"))) :not :to-be nil)

              ;; Links are updated with the appropriate name
              (expect (with-temp-buffer
                        (insert-file-contents (abs-path "f3.org"))
                        (buffer-string)) :to-match (regexp-quote "[[file:meaningful-title.org][meaningful-title]]"))))

(describe "delete file updates cache"
          (before-each
           (org-roam--test-init)
           (org-roam--clear-cache)
           (org-roam--test-build-cache))
          (it "delete f1"
              (delete-file (abs-path "f1.org"))
              (expect (->> (org-roam--forward-links-cache)
                           (gethash (abs-path "f1.org"))) :to-be nil)
              (expect (->> (org-roam--backward-links-cache)
                           (gethash (abs-path "nested/f1.org"))
                           (gethash (abs-path "f1.org"))) :to-be nil)
              (expect (->> (org-roam--backward-links-cache)
                           (gethash (abs-path "nested/f1.org"))
                           (gethash (abs-path "nested/f2.org"))) :not :to-be nil)))
