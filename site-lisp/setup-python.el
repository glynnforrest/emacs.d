(eval-when-compile (require 'use-package))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))

(provide 'setup-python)
