(require 'emamux)
(setq emamux:use-nearest-pane t)

;; Overriding this function so Emacs.app on OSX works.
(defun emamux:in-tmux-p () t)

;;; Bound to ,T
(require 'try-code)

(require 'setup-eshell)

(provide 'setup-programming)
