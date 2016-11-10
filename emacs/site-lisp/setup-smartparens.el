(use-package smartparens :ensure t
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :config
  (progn
    ;; settings
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)

    (add-hook 'prog-mode-hook 'smartparens-strict-mode)
    (add-hook 'comint-mode-hook 'smartparens-strict-mode)))


(provide 'setup-smartparens)
