(use-package yaml-mode :ensure t
  :mode "\\.yaml\\$"
  :config
  (setq yaml-indent-offset 4)

  (defun gf/yaml-toggle-indent-offset ()
    "Toggle between 4 and 2 spaces for indenting yaml files."
    (interactive)
    (if (eq yaml-indent-offset 4)
        (setq yaml-indent-offset 2)
      (setq yaml-indent-offset 4))
    (message "Yaml indentation is now %s spaces" yaml-indent-offset))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'yaml-mode-map
   "C" 'gf/php-insert-class
   "s" 'gf/php-insert-service)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'yaml-mode-map
   "ti" '(gf/yaml-toggle-indent-offset :which-key "toggle yaml indentation")))

(provide 'setup-yaml)
