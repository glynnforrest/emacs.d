(require 's)

(defvar gf/projects-file-override-alist '()
  "An association list of regex patterns and the project org file for them.

This enables overriding the default behaviour of `gf/projects--resolve-org-path'.

CAR must be a regular expression of files to match.
CDR must be a path to an org file, relative to `org-directory'.

Example:

\'((\"/home/emacs/some-company/some-project\" \"some-company.org\")
(\"some-company/different-project\" \"some-company.org\"))
")

(defun gf/projects--create-org-path (file)
  "Create a name suitable for an org file from the last part of a file
path."
  (let ((last (s-chop-prefix "." (car (last (split-string (s-chop-suffix "/" file) "/"))))))
    (concat (expand-file-name org-directory)
            (substring last 0 1)
            "/"
            (downcase
             (replace-regexp-in-string
              "\\." "-" last))
            ".org")))


(defun gf/projects--resolve-org-path (file)
  "Get the path of the project org file for the given file, either by creating a
suitable name automatically or matching a regex in gf/projects-file-override-alist."
  (let* ((resolved-file (if file file default-directory))
        (default-directory (file-name-directory resolved-file))
        (override (car (assoc-default resolved-file
                                      gf/projects-file-override-alist
                                      (lambda(regex org-file)
                                        (string-match regex resolved-file))))))
    (if override (concat org-directory (s-chop-prefix "/" override))
      (gf/projects--create-org-path (projectile-project-root)))))

(defun gf/projects-open-org-file ()
  "Open the org file for the current project."
  (interactive)
  (unless (gf/projects--in-project-file)
    (find-file (gf/projects--resolve-org-path (buffer-file-name)))))

(defvar gf/projects--previous-files (make-hash-table :test 'equal))

(defun gf/projects--in-project-file ()
  (and (string-equal "org-mode" (symbol-name major-mode)) (s-contains-p "/notes/" (buffer-file-name))))

(defun gf/projects--get-toggle-org-file ()
  (if (gf/projects--in-project-file)
      (let ((file (gethash (buffer-file-name) gf/projects--previous-files)))
        (if file file
          (error "Unable to determine previous project file.")))
    (let ((file (gf/projects--resolve-org-path (buffer-file-name))))
      (puthash file (buffer-file-name) gf/projects--previous-files)
      file)))

(defun gf/projects-toggle-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (find-file (gf/projects--get-toggle-org-file)))

(provide 'lib-projects)
