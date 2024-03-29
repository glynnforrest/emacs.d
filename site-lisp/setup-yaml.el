(eval-when-compile (require 'use-package))

(use-package yaml-mode
  :mode "\\.ya?ml.dist\\'"
  :config
  (setq-default yaml-indent-offset 4)

  (defun gf/yaml-toggle-indent-offset ()
    "Toggle between 4 and 2 spaces for indenting yaml files."
    (interactive)
    (if (eq (default-value 'yaml-indent-offset) 4)
        (setq-default yaml-indent-offset 2)
      (setq-default yaml-indent-offset 4))
    (message "Default yaml indentation is now %s spaces" (default-value 'yaml-indent-offset)))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'yaml-mode-map
   "ti" '(gf/yaml-toggle-indent-offset :which-key "yaml indentation")))

(provide 'setup-yaml)
