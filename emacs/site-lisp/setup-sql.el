(use-package sqlup-mode :ensure t
  :defer t
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps '(sql-mode-map sql-interactive-mode-map)
   "tu" '(sqlup-mode :which-key "toggle sqlup-mode")))

(provide 'setup-sql)
