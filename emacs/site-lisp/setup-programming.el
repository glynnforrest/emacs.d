(require 'autopair)
(setq autopair-blink nil)
(autopair-global-mode t)

;; (require 'smartparens)
;; (smartparens-mode t)

(require 'git-gutter)
(global-git-gutter-mode)

(require 'emamux)
(setq emamux:use-nearest-pane t)

;; Overriding this function so Emacs.app on OSX works.
(defun emamux:in-tmux-p () t)

;;; Bound to ,T
(require 'try-code)

;; toggle comments
(define-key evil-visual-state-map ",c"
  (lambda()
    (interactive)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (evil-visual-restore)))

(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(require 'setup-magit)
(require 'setup-eshell)
(require 'setup-flycheck)

(provide 'setup-programming)
