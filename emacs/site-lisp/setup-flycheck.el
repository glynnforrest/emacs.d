(use-package flycheck :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             php-phpmd
                                             php-phpcs
                                             scss)

                ;; so flycheck can check (require) calls properly.
                flycheck-emacs-lisp-load-path 'inherit)

  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-standard-error-navigation nil
        flycheck-highlighting-mode 'lines)

  (set-face-attribute 'flycheck-error nil
                      :foreground "#ffffff"
                      :background "#671232"
                      :underline nil)

  (set-face-attribute 'flycheck-warning nil
                      :foreground "#ceb4e2"
                      :background nil
                      :underline nil)

  (global-flycheck-mode))

  (provide 'setup-flycheck)
