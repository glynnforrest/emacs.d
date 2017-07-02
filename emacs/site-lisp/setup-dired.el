(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top))

(provide 'setup-dired)
