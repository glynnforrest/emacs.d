(require 'php-mode)

(add-hook 'php-mode-hook (lambda()
						   (setup-electric-semicolon php-mode-map)))

(provide 'init-php)
