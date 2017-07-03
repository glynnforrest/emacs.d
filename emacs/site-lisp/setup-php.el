(use-package php-mode :ensure t
  :defer t
  :config
  (require 'defuns-php)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'php-mode-map
   "c" 'gf/php-insert-use-class
   "C" 'gf/php-insert-class
   "s" 'gf/php-insert-service
   "r" 'gf/php-insert-symfony-route)

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
