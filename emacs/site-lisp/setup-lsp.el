(use-package lsp-mode
  :defer t
  :diminish lsp-mode
  :commands
  (lsp-mode lsp-define-stdio-client lsp-client-on-notification lsp-make-traverser)
  :init
  (setq lsp-enable-eldoc t)
  :config
  (require 'lsp-flycheck))

(provide 'setup-lsp)
