(require 'flycheck)

(global-flycheck-mode)

;; checkdoc is a bit intrusive for emacs lisp configs
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'php-phpmd flycheck-checkers)))
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'php-phpcs flycheck-checkers)))

(add-hook 'js-mode-hook (lambda ()
                         (interactive)
                         (flycheck-mode -1)))

(provide 'setup-flycheck)
