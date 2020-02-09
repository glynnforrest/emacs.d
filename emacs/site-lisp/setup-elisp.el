(use-package cask-mode)

(use-package lisp-mode
  :straight nil
  :config
  (general-define-key
   :keymaps 'lisp-mode-shared-map
   "M-RET" 'lisp-describe-thing-at-point))

(use-package el-autoyas
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

(use-package elisp-slime-nav
  :defer t
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'lisp-mode-shared-map
   "." 'elisp-slime-nav-find-elisp-thing-at-point
   "," 'pop-tag-mark))

(require 'defuns-elisp)

(provide 'setup-elisp)
