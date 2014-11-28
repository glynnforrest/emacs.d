
;; set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(
    ace-jump-mode
    ack-and-a-half
    apache-mode
    auto-complete
    autopair
    browse-kill-ring
    color-theme-sanityinc-tomorrow
    dired+
    el-autoyas
    elisp-slime-nav
    emmet-mode
    ethan-wspace
    evil
    evil-args
    evil-exchange
    evil-jumper
    emamux
    expand-region
    flycheck
    git-gutter
    helm
    helm-css-scss
    helm-dash
    helm-ls-git
    helm-projectile
    highlight-chars
    impatient-mode
    js-comint
    js2-mode
    js2-refactor
    keychain-environment
    magit
    markdown-mode
    multiple-cursors
    org
    paredit
    paredit-everywhere
    php-eldoc
    php-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    rotate-text
    scss-mode
    skewer-mode
    smartparens
    surround
    web-mode
    wgrep
    wgrep-ack
    yasnippet
    )
  "A list of required packages for this emacs configuration.")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Make sure stuff installed via homebrew is available
(push "/usr/local/bin" exec-path)


(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq plugins-dir (expand-file-name "plugins" user-emacs-directory))

(let ((default-directory plugins-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path site-lisp-dir)

;; Load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load personal configurations, like usernames and passwords
(require 'personal nil t)

;; Load appearance early to reduce flicker of default emacs
(require 'appearance)

;; Clearly necessary
(require 'setup-evil)
(require 'modes)
(require 'setup-autocomplete)
(require 'setup-general)
(require 'setup-whitespace)
(require 'setup-programming)
(require 'setup-projects)
(require 'setup-helm)
(require 'defuns)

(require 'help-mode)
(require 'setup-multiple-cursors)

(require 'setup-org)
(require 'setup-yasnippet)
(require 'setup-search)
(require 'mappings)

;; Mac specific configuration
(setq is-mac (equal system-type 'darwin))
(when is-mac (require 'setup-mac))
