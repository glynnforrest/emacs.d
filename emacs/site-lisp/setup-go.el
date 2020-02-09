(use-package go-mode
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    (require 'helm-dash)

    (defun gf/helm-dash-go ()
      (interactive)
      (setq-local helm-dash-docsets '("Go")))
    (add-hook 'go-mode-hook 'gf/helm-dash-go)))

(provide 'setup-go)
