(require 'flyspell)
(setq flyspell-issue-message-flag nil)

;; spell checking in certain modes
(dolist (hook '(org-mode-hook git-commit-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (auto-fill-mode 1)
                   )))

;; spell checking in comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; make sure spell checking works
(setq-default ispell-program-name "aspell")

(provide 'setup-flyspell)
