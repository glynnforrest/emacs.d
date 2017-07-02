(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  ;; Don't use bundled snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; don't expand part of words
  (setq yas-key-syntaxes '("w_" "w_." "^ "))
  (yas-global-mode 1))

(provide 'setup-yasnippet)
