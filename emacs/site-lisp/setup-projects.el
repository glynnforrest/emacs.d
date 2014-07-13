(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm-comp-read)

(setq org-projects-dir "~/notes/projects")

(defun gf/create-org-path (path)
  "Create a name suitable for an org file from the last part of a file
path."
  (let ((last (car (last (split-string (if (equal (substring path -1) "/")
                            (substring path 0 -1) path) "/")))))
    (downcase
     (concat org-projects-dir "/"
             (replace-regexp-in-string
              "\\." "-" (if (equal (substring last 0 1) ".")
                            (substring last 1) last))
             ".org"))))

(defun gf/switch-to-project-org-file ()
  "Switch to the org file for the current project."
  (interactive)
  (find-file (gf/create-org-path (projectile-project-root))))

(defun gf/toggle-switch-to-project-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (if (string-equal "org-mode" (symbol-name major-mode))
      (previous-buffer)
    (gf/switch-to-project-org-file)))

(provide 'setup-projects)
