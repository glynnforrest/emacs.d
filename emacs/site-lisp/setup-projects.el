(use-package projectile
  :diminish ""
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  (require 'defuns-projects))

(provide 'setup-projects)
