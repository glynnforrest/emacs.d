;; node REPL
(require 'js-comint)
(require 'js2-mode)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 node")

(setq js2-mode-hook (lambda ()
                      (flycheck-mode -1)
                      (setq indent-tabs-mode nil)
                      (setq js2-basic-offset 2)))

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix ",l")

(provide 'setup-js)
