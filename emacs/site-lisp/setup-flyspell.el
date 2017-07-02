(use-package flyspell :ensure t
  :diminish "spell"
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (mapc (lambda (hook)
          (add-hook hook #'flyspell-mode))
        '(org-mode-hook
          with-editor-mode-hook
          rst-mode-hook))
  :config
  (setq flyspell-issue-message-flag nil)
  (setq-default ispell-program-name "ispell")
  (ispell-change-dictionary "english" t))

(use-package helm-flyspell :ensure t
  :after (helm flyspell)
  :config
  (general-define-key
   :keymaps 'evil-normal-state-map
   "z=" 'helm-flyspell-correct))

(provide 'setup-flyspell)
