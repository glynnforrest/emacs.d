(require 'flycheck)

(global-flycheck-mode)

;; checkdoc is a bit intrusive for emacs lisp configs
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'php-phpmd flycheck-checkers)))
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'php-phpcs flycheck-checkers)))

(add-hook 'js-mode-hook (lambda ()
                         (interactive)
                         (flycheck-mode -1)))

(setq flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-standard-error-navigation nil)

(setq flycheck-highlighting-mode 'lines)

(set-face-attribute 'flycheck-error nil
                    :background "red"
                    :underline nil)

(set-face-attribute 'flycheck-warning nil
                    :foreground nil
                    :underline t)

(provide 'setup-flycheck)
