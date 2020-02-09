(use-package hcl-mode :ensure t)

(use-package terraform-mode
  :ensure t
  :config

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'terraform-mode-map
   "+" 'terraform-format-buffer)
  )

(provide 'setup-hashicorp)
