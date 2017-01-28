(use-package projectile :ensure t
  :diminish ""
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode))

(provide 'setup-projects)
