(require 'web-mode)

;; Use multiple-cursors to rename tags
(define-key web-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)

;; Sort of like matchit
(evil-declare-key 'normal web-mode-map (kbd "%") 'web-mode-tag-match)

(provide 'setup-web-mode)
