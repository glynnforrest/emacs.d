(eval-when-compile (require 'use-package))

(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'eglot-ensure))

(provide 'setup-python)
