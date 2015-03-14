;; Emacs lisp
(require 'setup-elisp)

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(require 'setup-js)

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (define-key markdown-mode-map (kbd "<tab>") 'yas/expand)))
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Salt
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; PHP
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(require 'setup-php)

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; lilypond
(autoload 'LilyPond-mode "lilypond-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (require 'setup-lilypond)))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; Others
(require 'setup-css)
(require 'setup-web-mode)

(provide 'modes)
