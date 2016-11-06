(use-package magit :ensure t
  :config
  (setq magit-log-arguments '("-n256" "--decorate"))
  (setq inhibit-magit-revert t)
  )

(provide 'setup-git)
