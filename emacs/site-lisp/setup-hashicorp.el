(eval-when-compile (require 'use-package))
(eval-when-compile (require 'general))
(eval-when-compile (require 'eglot))

(use-package hcl-mode
  :mode "\\.hcl2\\'")

(use-package terraform-mode

  :config
  (add-to-list 'eglot-server-programs '(terraform-mode "/usr/local/Cellar/terraform-ls/0.31.1/bin/terraform-ls" "serve"))
  (add-hook 'terraform-mode-hook 'eglot-ensure)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'terraform-mode-map
   "+" 'terraform-format-buffer)
  )

(provide 'setup-hashicorp)
