(when (not (version< emacs-version "25.1"))
  (use-package lsp-mode
    :defer t
    :diminish lsp-mode
    :commands
    (lsp-mode lsp-define-stdio-client lsp-client-on-notification lsp-make-traverser)
    :init
    (setq lsp-enable-eldoc t))
  (use-package lsp-ui
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(provide 'setup-lsp)
