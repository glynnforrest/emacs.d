(use-package dired
  :straight nil
  :defer t
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil)
  (general-define-key
   :keymaps 'dired-mode-map
   "M-e" 'wdired-change-to-wdired-mode
   "SPC" nil))

(require 'setup-os)
(use-package dired-rsync
  :config
  (when gf/is-mac
    (setq dired-rsync-options "-az --progress"))

  (general-define-key
   :keymaps 'dired-mode-map
   "]" 'dired-rsync
   "[" (gf/key 'dired-rsync "~/Desktop")))

(provide 'setup-dired)
