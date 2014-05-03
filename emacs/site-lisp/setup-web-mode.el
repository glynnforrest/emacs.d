(require 'web-mode)

;; Use multiple-cursors to rename tags
(define-key web-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)

;; Sort of like matchit
(evil-declare-key 'normal web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'visual web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'normal web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)
(evil-declare-key 'visual web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)

(require 'scss-mode)
(setq scss-compile-at-save nil)

(provide 'setup-web-mode)
