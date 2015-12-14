;; node REPL
(require 'js-comint)
(require 'js2-mode)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 node")

(setq js2-mode-hook (lambda ()
                      (flycheck-mode -1)))

(defun gf/js2-mode-toggle-indent-offset ()
  "Switch between 2 and 4 spaces for javascript indentation"
  (interactive)
  (if (eq js2-basic-offset 2)
      (setq js2-basic-offset 4)
    (setq js2-basic-offset 2))
  (message (format "Javascript indendation is now %s spaces" js2-basic-offset)))

(setq js2-basic-offset 2)

(require 'js2-refactor)

(provide 'setup-js)
