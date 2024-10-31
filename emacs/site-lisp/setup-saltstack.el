(eval-when-compile (require 'use-package))

(use-package salt-mode
  :config
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1)))

  (general-define-key
   :states '(normal)
   :keymaps 'salt-mode-map
   "K" 'salt-mode-browse-doc))

(provide 'setup-saltstack)
