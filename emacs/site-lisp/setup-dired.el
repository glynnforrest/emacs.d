(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (general-define-key
   :keymaps 'dired-mode-map
   "M-e" 'wdired-change-to-wdired-mode
   "SPC" nil))

(require 'setup-os)
(use-package dired-rsync :ensure t
  :config
  (when gf/is-mac
    (setq dired-rsync-options "-az --progress"))

  (general-define-key
   :keymaps 'dired-mode-map
   "]" 'dired-rsync
   "[" (gf/key 'dired-rsync (expand-file-name "~/Desktop"))))

(provide 'setup-dired)
