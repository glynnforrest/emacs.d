(use-package projectile :ensure t
  :diminish ""
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  (require 'defuns-projects))

(provide 'setup-projects)
