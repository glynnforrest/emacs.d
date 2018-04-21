(require 'help-mode)
(evil-set-initial-state 'help-mode 'normal)
(general-define-key
 :states '(normal)
 :keymaps 'help-mode-map
 "q" 'quit-window)

(provide 'setup-help)
