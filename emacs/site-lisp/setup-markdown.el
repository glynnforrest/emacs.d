(use-package markdown-mode :ensure t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
(add-hook 'markdown-mode-hook 'flyspell-mode))

(provide 'setup-markdown)