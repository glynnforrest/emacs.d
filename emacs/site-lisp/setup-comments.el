(use-package evil-nerd-commenter :ensure t
  :defer t
  :commands (evilnc-comment-operator)
  :config
  (general-define-key
   :states '(normal visual)
   "gc" 'evilnc-comment-operator))

(provide 'setup-comments)