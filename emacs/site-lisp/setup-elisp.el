(use-package cask-mode :ensure t)

(use-package lisp-mode
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'lisp-mode-shared-map
   "M-RET" 'lisp-describe-thing-at-point))

(use-package el-autoyas :ensure t
  :defer t
  :commands (el-autoyas-enable)
  :init
  (add-hook 'emacs-lisp-mode-hook 'el-autoyas-enable)
  (add-hook 'lisp-interaction-mode-hook 'el-autoyas-enable)
  (add-hook 'ielm-mode-hook 'el-autoyas-enable))

(use-package eldoc
  :defer t
  :diminish ""
  :commands (eldoc-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

(use-package elisp-slime-nav :ensure t
  :defer t
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(require 'defuns-elisp)

(provide 'setup-elisp)
