(use-package helm-gtags :ensure t
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor nil)

  (require 'projectile)

  (defun gf/gtags-generate-for-project ()
    "Run gtags in the current project root directory."
    (interactive)
    (let ((dir (projectile-project-root)))
      (start-process-shell-command "gtags" "*GTAGS*" (concat "(cd " dir "; gtags -v --gtagslabel pygments)"))
      (message (concat "Generating gtags in " dir))))

  (defun gf/gtags-delete-for-project ()
    "Delete all gtag files in the current project directory."
    (interactive)
    (let ((dir (projectile-project-root)))
      (shell-command (concat "find " dir " -type f \\( -name GPATH -or -name GRTAGS -or -name GTAGS \\) -delete"))
      (message (concat "Removed gtags in " dir)))))

(provide 'setup-gtags)
