(eval-when-compile (require 'use-package))

(use-package php-mode
  :defer t
  :mode "\\.php$"
  :commands (gf/php-insert-class
             gf/php-insert-service
             gf/php-insert-route
             gf/php-show-date-format-help)
  :config
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
  (require 'lib-php)

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
   :prefix gf/leader-key
   :non-normal-prefix gf/non-normal-leader-key
   :keymaps 'php-mode-map
   "+" 'gf/php-cleanup-style))

(provide 'setup-php)
