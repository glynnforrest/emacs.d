(use-package php-mode :ensure t
  :defer t
  :mode "\\.php$"
  :commands (gf/php-insert-class
             gf/php-insert-service
             gf/php-insert-route)
  :config
  (require 'defuns-php)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'php-mode-map
   "c" 'gf/php-use-class-select
   "r" 'gf/php-refresh-class-candidates
   "t" 'gf/php-use-trait-select)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'php-mode-map
   "+" 'gf/php-cleanup-style)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'php-mode-map
   "C-c l" 'yas/create-php-snippet)

  (defun gf/helm-dash-php ()
    (interactive)
    (setq-local helm-dash-docsets '("PHP" "PHPUnit" "Symfony")))
  (add-hook 'php-mode-hook 'gf/helm-dash-php))

(use-package php-auto-yasnippets :ensure t
  :after php-mode)

(provide 'setup-php)
