(use-package helm-gtags :ensure t
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor nil))

(provide 'setup-gtags)
