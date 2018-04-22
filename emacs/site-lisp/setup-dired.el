(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (general-define-key
   :keymaps 'dired-mode-map
   "SPC" nil))

(provide 'setup-dired)
