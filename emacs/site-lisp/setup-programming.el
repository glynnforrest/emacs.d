(require 'emamux)
(setq emamux:use-nearest-pane t)

;; Overriding this function so Emacs.app on OSX works.
(defun emamux:in-tmux-p () t)

(require 'setup-eshell)

(provide 'setup-programming)
