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
       (org-mode)
       (insert "#+title:normal title")
       (org-roam-db--file-title))
     :to-equal "normal title"))
  (it "supports multi-line titles"
    (expect
     (with-temp-buffer
       (org-mode)
       (insert "#+title: title:\n#+title: separated by newline")
       (org-roam-db--file-title))
     :to-equal "title: separated by newline"))
  (it "supports file-name based titles"
    (progn
      (setq org-roam-directory temporary-file-directory
            org-roam-db-location (expand-file-name "org-roam.db" temporary-file-directory)
            org-roam-file-extensions '("org"))
      (with-temp-buffer
        (org-mode)
        (write-file (expand-file-name "test file.org" org-roam-directory))
        (org-roam-db--file-title)))
    :to-equal "test file"))

(defun test-org-roam-utils--kinda-expensive-func ()
  (prin1-to-string (list 'foo 'bar (* 234.2 345.0) (/ 255.2 2123.1))))

(describe "org-roam--memoize"
  :var (;; Different KEY every call.
        (dur-memoized-n-times
         (car (benchmark-run-compiled 1
               (dotimes (i 1000)
                 (org-roam--memoize i
                   (test-org-roam-utils--kinda-expensive-func))))))

        ;; Same KEY every call.
        (dur-memoized-once
         (car (benchmark-run-compiled 1
               (dotimes (_ 1000)
                 (org-roam--memoize :foo
                   (test-org-roam-utils--kinda-expensive-func))))))

        (dur-unmemoized
         (car (benchmark-run-compiled 1
                (dotimes (_ 1000)
                  (test-org-roam-utils--kinda-expensive-func))))))

  ;; Likely plenty better than 10x, but we leave some headroom in this test.
  (it "shortens 1000 calls to have the runtime of at most 100 calls"
    (expect dur-unmemoized :to-be-greater-than (* 10 dur-memoized-once)))

  ;; This case is why `org-roam--memoize' contains a check for
  ;; (memq org-roam--memo-timer timer-list), or it would be much, much worse.
  (it "is not much slower than unmemoized, when KEY is new each time"
    (expect (* 10 dur-unmemoized) :to-be-greater-than dur-memoized-n-times)))

(provide 'test-org-roam-utils)

;;; test-org-roam-utils.el ends here
