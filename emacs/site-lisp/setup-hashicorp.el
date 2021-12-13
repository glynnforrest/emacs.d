(eval-when-compile (require 'use-package))

(use-package hcl-mode
  :mode "\\.hcl2\\'")

(use-package terraform-mode

  :config

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'terraform-mode-map
   "+" 'terraform-format-buffer)
  )

(provide 'setup-hashicorp)
