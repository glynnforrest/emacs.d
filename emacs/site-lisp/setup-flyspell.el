(use-package flyspell
  :diminish "spell"
  :defer t
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (mapc (lambda (hook)
          (add-hook hook #'flyspell-mode))
        '(org-mode-hook
          with-editor-mode-hook
          rst-mode-hook))
  :config
  (setq flyspell-issue-message-flag nil))

(use-package helm-flyspell
  :after (helm flyspell)
  :config
  (general-define-key
   :keymaps 'evil-normal-state-map
   "z=" 'helm-flyspell-correct)
  (general-define-key
   "M-;" 'helm-flyspell-correct))

(provide 'setup-flyspell)
