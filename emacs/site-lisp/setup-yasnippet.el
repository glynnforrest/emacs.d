(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  ;; Don't use bundled snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; don't expand part of words
  (setq yas-key-syntaxes '("w_" "w_." "^ "))
  (yas-global-mode 1)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'snippet-mode-map
   "t" 'yas-tryout-snippet))

(provide 'setup-yasnippet)
