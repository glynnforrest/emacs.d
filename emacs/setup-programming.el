;; use spaces by default
(setq-default
 c-basic-offset 4
 tab-width 4
 )

;; (require 'autopair)
;; (setq autopair-blink nil)
;; (autopair-global-mode t)

(require 'smartparens)
(smartparens-mode t)

(require 'git-gutter)
(global-git-gutter-mode)

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

(add-hook 'prog-mode-hook
          (progn
            (flyspell-prog-mode)
            (flyspell-buffer)))

(require 'setup-magit)
(require 'setup-js)
(require 'setup-css)
(require 'setup-php)
(require 'setup-eshell)
(require 'setup-web-mode)
(require 'test-case-mode)
(require 'setup-flycheck)

(provide 'setup-programming)
