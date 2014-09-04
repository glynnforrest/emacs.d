(require 'projectile)
(projectile-global-mode)

(setq helm-projectile-sources-list '(helm-source-projectile-buffers-list helm-source-projectile-files-list))

(defvar org-projects-dir (expand-file-name  "~/notes/projects"))

(defun gf/create-org-path (path)
  "Create a name suitable for an org file from the last part of a file
path."
  (let ((last (car (last (split-string (if (equal (substring path -1) "/")
                                           (substring path 0 -1) path) "/")))))
    (concat org-projects-dir "/"
            (downcase
             (replace-regexp-in-string
              "\\." "-" (if (equal (substring last 0 1) ".")
                            (substring last 1) last)))
            ".org")))

(defun gf/project-org-file ()
  "Get the path of the org file for the current project."
  (gf/create-org-path (projectile-project-root)))

(defun gf/switch-to-project-org-file ()
  "Switch to the org file for the current project."
  (interactive)
  (find-file (gf/project-org-file)))

(defvar gf/previous-project-buffers (make-hash-table :test 'equal))

(defun gf/toggle-switch-to-project-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (if (string-equal "org-mode" (symbol-name major-mode))
      (if (gethash (buffer-file-name) gf/previous-project-buffers)
          (switch-to-buffer (gethash (buffer-file-name) gf/previous-project-buffers))
        (error "Previous project buffer not found"))
    (let ((file (gf/project-org-file)))
      (puthash file (current-buffer) gf/previous-project-buffers)
      (find-file file)
      )))

(provide 'setup-projects)
