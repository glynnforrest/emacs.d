(eval-when-compile (require 'use-package))

(use-package go-mode
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'go-mode-map
   "a" 'go-import-add))

(provide 'setup-go)
