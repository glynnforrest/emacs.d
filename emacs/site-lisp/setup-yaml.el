(require 'yaml-mode)

(setq yaml-indent-offset 4)

(define-key yaml-mode-map (kbd "C-c I") 'gf/php-insert-class)
(define-key yaml-mode-map (kbd "C-c s") 'gf/php-insert-service)

(provide 'setup-yaml)
