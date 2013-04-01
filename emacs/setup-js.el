;; node REPL
(require 'js-comint)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 node")

(provide 'setup-js)
