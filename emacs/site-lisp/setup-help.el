(evil-set-initial-state 'help-mode 'normal)

(general-define-key
 :states '(normal)
 :keymaps 'help-mode-map
 "q" 'quit-window)

(general-define-key
 :keymaps 'help-mode-map
 "o" 'ace-link-help)

(general-define-key
 :keymaps 'Info-mode-map
 "o" 'ace-link-info)

(provide 'setup-help)
