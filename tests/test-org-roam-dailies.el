;;; test-org-roam-dailies.el --- NONDESCRIPT FILE  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'org-roam)
(require 'org-roam-dailies)

;; This is indirectly a test of func `org-roam-capture-find-or-create-olp'
;; from org-roam-capture.el.
(describe "org-roam-capture-find-or-create-olp"
  (before-all
    (setq org-roam-directory (expand-file-name "tests/roam-files")
          org-roam-dailies-directory "dailies/"
          ;; Remember to clean up dailies/test.org lest it break other tests!
          org-roam-dailies-capture-templates
          `(("t" "Tasks" entry "* %?"
             :target (file+head+olp "test.org" ;; "%<%Y-%m-%d>.org"
                                    "* Notes\n\n* Tasks\n\n* Journal"
                                    ("Tasks" "[2023-03-28 Tue]")))))
    (org-roam-db-sync))

  (it "does not create Tasks heading multiple times (issue #2335)"
    (let (buf)
      (dotimes (_ 3)
        (save-window-excursion
          (setq buf (current-buffer))
          (org-roam-dailies--capture (current-time) t "t")))
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (expect (cl-loop while (search-forward "\n* Tasks" nil t) count t)
                :to-equal 1)
        (kill-buffer))
      (delete-file "tests/roam-files/dailies/test.org"))))

(provide 'test-org-roam-dailies)

;;; test-org-roam-dailies.el ends here
