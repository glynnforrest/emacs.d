(use-package emmet-mode :ensure t
  :defer t

  :init
  (mapc (lambda(hook)
            (add-hook hook 'emmet-mode))
          '(html-mode-hook web-mode-hook css-mode-hook))

  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding t))

(provide 'setup-emmet)
