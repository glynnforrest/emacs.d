(require 'flycheck)

(global-flycheck-mode)

;; checkdoc is a bit intrusive for emacs lisp configs
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(provide 'setup-flycheck)
