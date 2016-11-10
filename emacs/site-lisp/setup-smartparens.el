(use-package smartparens :ensure t
  :commands (sp-split-sexp sp-newline sp-up-sexp)

  :init
  (add-hook 'prog-mode-hook 'smartparens-strict-mode)
  (add-hook 'comint-mode-hook 'smartparens-strict-mode)

  :config
  (progn
    ;; settings
    (require 'smartparens-config)
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)))

(provide 'setup-smartparens)
