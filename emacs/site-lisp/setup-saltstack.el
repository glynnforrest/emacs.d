(use-package salt-mode
  :config
  (defun gf/helm-dash-saltstack ()
    (interactive)
    (setq-local helm-dash-docsets '("SaltStack")))
  (add-hook 'salt-mode-hook 'gf/helm-dash-saltstack)
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1)))

  (general-define-key
   :states '(normal)
   :keymaps 'salt-mode-map
   "K" 'salt-mode-browse-doc))

(provide 'setup-saltstack)
