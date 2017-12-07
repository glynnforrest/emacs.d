(use-package go-mode :ensure t
  :config
  (progn
      (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(provide 'setup-go)
