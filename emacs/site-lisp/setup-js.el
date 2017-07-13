(use-package js2-mode :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  :config

  (add-hook 'js2-mode-hook (lambda ()
                             (flycheck-mode -1)))

  (setq-default js2-basic-offset 2)

  (defun gf/js2-mode-toggle-indent-offset ()
    "Switch between 2 and 4 spaces for javascript indentation"
    (interactive)
    (if (eq (default-value 'js2-basic-offset) 4)
        (setq-default js2-basic-offset 2)
      (setq-default js2-basic-offset 4))
    (message (format "Default javascript indentation is now %s spaces" (default-value 'js2-basic-offset))))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'js2-mode-map
   "ti" '(gf/js2-mode-toggle-indent-offset :which-key "javascript indentation")))

(use-package js2-refactor :ensure t
  :after js2-mode)

(use-package js-comint :ensure t
  :config
  (setq inferior-js-program-command "env NODE_NO_READLINE=1 node")) 

(provide 'setup-js)
