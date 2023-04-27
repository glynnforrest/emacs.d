(eval-when-compile (require 'use-package))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t)

  (general-define-key
   :keymaps 'markdown-mode-map
   "M-h" 'markdown-promote-subtree
   "M-l" 'markdown-demote-subtree
   "M-H" 'markdown-promote
   "M-L" 'markdown-demote
   "M-RET" 'markdown-insert-header-dwim)

  (general-define-key
   :keymaps 'markdown-mode-map
   :states 'normal
   "TAB" 'outline-cycle)

  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'ws-butler-mode)
  (add-hook 'markdown-mode-hook 'outline-minor-mode))

(provide 'setup-markdown)
