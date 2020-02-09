(use-package ace-link
  :config
  (with-eval-after-load 'info
     (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'ace-link-help)))

(provide 'setup-ace-link)
