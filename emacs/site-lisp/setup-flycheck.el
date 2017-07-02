(use-package flycheck :ensure t
  :diminish ""
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

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode rst-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  (global-flycheck-mode))


(provide 'setup-flycheck)
