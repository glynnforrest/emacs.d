(use-package hcl-mode)

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
