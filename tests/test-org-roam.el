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

(defun test-org-roam--abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (expand-file-name file-path org-roam-directory))

(defun test-org-roam--find-file (path)
  "PATH."
  (let ((path (test-org-roam--abs-path path)))
    (make-directory (file-name-directory path) t)
    (find-file path)))

(defvar test-org-roam-directory (expand-file-name "tests/roam-files")
  "Directory containing org-roam test org files.")

(defun test-org-roam--init ()
  "."
  (let ((original-dir test-org-roam-directory)
        (new-dir (expand-file-name (make-temp-name "org-roam") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-setup)
    (sleep-for 2)))

(defun test-org-roam--teardown ()
  (org-roam-teardown)
  (delete-file org-roam-db-location)
  (org-roam-db--close))

(describe "Ref extraction"
  (before-all
    (test-org-roam--init))

  (after-all
    (test-org-roam--teardown))

  (cl-flet
      ((test (fn file)
             (let* ((fname (test-org-roam--abs-path file))
                    (buf (find-file-noselect fname)))
               (with-current-buffer buf
                 ;; Unlike tag extraction, it doesn't make sense to
                 ;; pass a filename.
                 (funcall fn)))))
    ;; Enable "cite:" link parsing
    (org-link-set-parameters "cite")))

;;; Tests
(xdescribe "org-roam-db-sync"
  (before-each
    (test-org-roam--init))

  (after-each
    (test-org-roam--teardown))

  (it "initializes correctly"
    ;; Cache
    ;; TODO: Write tests

    (expect (org-roam-db-query [:select * :from refs])
            :to-have-same-items-as
            (list (list "https://google.com/" (test-org-roam--abs-path "web_ref.org") "website")))

    ;; Expect rebuilds to be really quick (nothing changed)
    (expect (org-roam-db-sync)
            :to-equal
            (list :files 0 :links 0 :tags 0 :titles 0 :refs 0 :deleted 0))))

(provide 'test-org-roam)

;;; test-org-roam.el ends here
