(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

;; Core stuff
(require 'setup-core)
(require 'setup-evil)
(require 'setup-keys)
(require 'setup-appearance)

;; Everything else
(require 'setup-ace-link)
(require 'setup-apache)
(require 'setup-coffeescript)
(require 'setup-comments)
(require 'setup-company)
(require 'setup-css)
(require 'setup-defuns)
(require 'setup-dired)
(require 'setup-docker)
(require 'setup-edit-server)
(require 'setup-elisp)
(require 'setup-emmet)
(require 'setup-flycheck)
(require 'setup-flyspell)
(require 'setup-git)
(require 'setup-go)
(require 'setup-groovy)
(require 'setup-helm)
(require 'setup-helm-dash)
(require 'setup-help)
(require 'setup-lsp)
(require 'setup-js)
(require 'setup-markdown)
(require 'setup-misc)
(require 'setup-org)
(require 'setup-os)
(require 'setup-php)
(require 'setup-projects)
(require 'setup-rotate-text)
(require 'setup-saltstack)
(require 'setup-shell-script)
(require 'setup-smartparens)
(require 'setup-sql)
(require 'setup-tmux)
(require 'setup-try-code)
(require 'setup-web-mode)
(require 'setup-which-key)
(require 'setup-whitespace)
(require 'setup-yaml)
(require 'setup-yasnippet)

;; Personal config if available, like usernames and passwords
(require 'setup-personal nil t)

;; Load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Quarantine - old files that need updating
;; (require 'setup-general)
;; (require 'defuns)
;; (require 'mappings)
;; (require 'appearance)
;; (require 'setup-erc)
;; (require 'setup-eshell)
;; (require 'setup-lilypond)
;; (require 'setup-multiple-cursors)

;; Quarantined packages
;; ace-jump-mode
;; dired+
;; epl
;; impatient-mode
;; keychain-environment
;; php-eldoc
