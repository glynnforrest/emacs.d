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
    elscreen
    evil
    flycheck
    js2-mode
    js-comint
    magit
    markdown-mode
    multiple-cursors
    multi-web-mode
    org
    php+-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    smex
    surround
    test-case-mode
    wgrep
    yasnippet
    zencoding-mode
    )
  "A list of required packages for this emacs configuration.")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(require 'personal)

;; Clearly necessary
(require 'setup-evil)
(require 'modes)
(require 'setup-autocomplete)
(require 'setup-general)
(require 'setup-programming)
(require 'setup-projects)
(require 'defuns)


(require 'help-mode)
;; (require 'setup-multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(require 'setup-org)
(require 'setup-yasnippet)
(require 'setup-occur-grep-ack)
(require 'appearance)
(require 'mappings)
