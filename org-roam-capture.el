(require 'org)
(require 'dash)

(defvar org-roam-capture-title nil
  "Current title for org-roam capture")

(defvar org-roam-default-filename "%(format-time-string \"%Y%m%d%H%M%S\" (current-time))-${title}"
  "Default filename for org-roam files.")

(defvar org-roam-templates
  (list (list :shortcode "d"
              :word "default"
              :template '("#+TITLE: TODO"
                          ""
                          ":META:"
                          ":TAGS: %^{tags}"
                          ":END:"
                          "%?")))
  "Templates to insert for new files in org-roam.")

(defun org-roam--format-file-name (str title)
  "Return a file name from STR.
The %-escapes from `org-capture' are allowed, see the documentation for more
details. In addition, the following ${} escapes are supported:

   ${title}    The TITLE of the file."
  (-> str
      (s-format 'aget
                (list (cons "title" (org-roam--title-to-slug title))))
      (org-capture-fill-template) ;; Abuse to expand template
      (s-trim)))

(defun org-roam-expand-template (template title &optional rest)
  "Expand a TEMPLATE into an `org-capture' compatible template.
TITLE is used to determine the new file name."
  (let* ((file-str (org-roam--format-file-name
                    (or (plist-get template :filename)
                        org-roam-default-filename) title))
         (file-path (org-roam--new-file-path file-str t)))
    (list (plist-get template :shortcode)
          (plist-get template :word)
          'plain
          (list 'file file-path)
          (s-join "\n" (plist-get template :template))
          :jump-to-captured t)))

(defun org-roam-capture (title &optional goto keys)
  "Org-roam capture templates."
  (interactive "sTitle: \nP")
  (let ((org-capture-templates (-map (lambda (template)
                                       (org-roam-expand-template template title))
                                     org-roam-templates)))
    (org-capture goto keys)))
