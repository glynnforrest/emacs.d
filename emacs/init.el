
;; set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(
    ace-jump-mode
    ack-and-a-half
    auto-complete
    autopair
    browse-kill-ring
    color-theme-sanityinc-tomorrow
    dired+
    el-autoyas
    elscreen
    evil
    expand-region
    flx-ido
    flycheck
    git-gutter
    js2-mode
    js-comint
    ido-vertical-mode
    impatient-mode
    keychain-environment
    magit
    markdown-mode
    multiple-cursors
    multi-web-mode
    org
    paredit
    paredit-everywhere
    php-eldoc
    php-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    rotate-text
    skewer-mode
    smartparens
    smex
    surround
    test-case-mode
    web-mode
    wgrep
    wgrep-ack
    yasnippet
    zencoding-mode
    )
  "A list of required packages for this emacs configuration.")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Make sure stuff installed via homebrew is available
(push "/usr/local/bin" exec-path)

;; Set path to .emacs.d
(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))

;; Set path to manually installed plugins
(setq plugins-dir (expand-file-name "plugins" emacs-dir))


;; Set up load path
(let ((default-directory plugins-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path emacs-dir)

;; Load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load personal configurations, like usernames and passwords
(require 'personal nil t)

;; Clearly necessary
(require 'setup-evil)
(require 'modes)
(require 'setup-autocomplete)
(require 'setup-general)
(require 'setup-whitespace)
(require 'setup-programming)
(require 'setup-projects)
(require 'defuns)

(require 'help-mode)
(require 'setup-multiple-cursors)

(require 'setup-org)
(require 'setup-yasnippet)
(require 'setup-occur-grep-ack)
(require 'appearance)
(require 'mappings)
