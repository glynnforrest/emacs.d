(eval-when-compile (require 'use-package))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t))


(defun gf/trim-ui ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode fringe-mode))
    (when (fboundp mode) (funcall mode -1))))

(gf/trim-ui)
(add-hook 'after-make-frame-functions (lambda(frame)
                                        (gf/trim-ui)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :init
  (mapc (lambda (hook)
          (add-hook hook 'rainbow-mode))
        '(css-mode-hook
          emacs-lisp-mode-hook
          haskell-mode-hook))
  :config
  (diminish 'rainbow-mode))

(use-package spaceline-config
  :straight spaceline
  :config

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-evil-state-faces (append spaceline-evil-state-faces
                                           '((lisp . spaceline-evil-motion))))

  (spaceline-toggle-flycheck-info-off)
  (spaceline-toggle-flycheck-error-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-spacemacs-theme))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package hydra )
(use-package diminish )

(use-package tramp)

;; enable/disable built-in modes
(blink-cursor-mode -1)
(column-number-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(recentf-mode t)
(savehist-mode t)
(show-paren-mode t)
(tooltip-mode -1)
(winner-mode t)
(xterm-mouse-mode -1)

(use-package autorevert
  :diminish ""
  :config
  (global-auto-revert-mode t))

;; sensible defaults 
(fset 'yes-or-no-p 'y-or-n-p)
(setq
 auto-revert-verbose nil
 auto-save-default nil
 create-lockfiles nil
 echo-keystrokes 0.1
 global-auto-revert-non-file-buffers t
 history-length 1000
 hl-line-sticky-flag t
 inhibit-startup-message t
 line-spacing 0
 make-backup-files nil
 mouse-wheel-progressive-speed nil
 recentf-max-saved-items 2000
 ring-bell-function #'ignore
 scroll-conservatively 10000
 scroll-step 1
 select-enable-clipboard t
 shift-select-mode nil
 undo-tree-visualizer-timestamps 1
 vc-follow-symlinks t)

;; enable normally disabled functions
(put 'narrow-to-region 'disabled nil)

(use-package restart-emacs)

(use-package s)

(use-package evil
  :init
  (setq-default
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t)

  :config
  (evil-mode 1)
  (use-package evil-numbers)

  ;; Centre screen around a search
  (defadvice
      evil-search-forward
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-search-forward)

  (defadvice
      evil-search-next
      (after evil-search-next-recenter activate)
    (recenter))
  (ad-activate 'evil-search-next)

  (defadvice
      evil-search-previous
      (after evil-search-previous-recenter activate)
    (recenter))
  (ad-activate 'evil-search-previous)

  (defun gf/visual-shift-left ()
    "Shift left and keep the region active."
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun gf/visual-shift-right ()
    "Shift right and keep the region active."
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

  (add-to-list 'evil-emacs-state-modes 'package-menu-mode)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'org-capture-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'occur-mode 'normal))

;; Save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t))

;; To not miss surround mode
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package undo-tree
  :diminish "")

(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t
              evil-lisp-state-enter-lisp-state-on-command nil))

(use-package expand-region
  :commands er/expand-region
  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "0"))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; exchange two regions or motions with gx. gX cancels a pending swap
(use-package evil-exchange
  :config
  (evil-exchange-install))

(defun gf/evil-forward-arg (count)
  "Small wrapper around evil-forward-arg when at the opening bracket."
  (interactive "p")
  (if (looking-at-p "(")
      (forward-char))
  (evil-forward-arg count)
  )

;; bind evil-forward/backward-args
;; (define-key evil-normal-state-map "L" 'gf/evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

(defvar gf/is-mac (equal system-type 'darwin))
(defvar gf/is-linux (equal system-type 'gnu/linux))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package projectile
  :diminish ""
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  (require 'defuns-projects))

;; https://tuhdo.github.io/helm-intro.html has great helm tips.
(use-package helm
  :diminish ""
  :commands helm
  :config

  (setq helm-move-to-line-cycle-in-source nil
        helm-split-window-default-side 'other
        helm-split-window-in-side-p t
        helm-display-header-line nil
        helm-candidate-number-limit 200
        helm-M-x-requires-pattern 0
        helm-net-prefer-curl-p t
        helm-buffer-max-length nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)

  (set-face-attribute 'helm-source-header nil :height 0.1)

  (helm-mode)
  (helm-autoresize-mode t)

  (defun gf/helm-find-in-directory (start)
    (interactive)
    (let ((default-directory (expand-file-name start)))
      (call-interactively 'helm-find-files)))

  (general-define-key
   :keymaps 'helm-map
   "C-j" 'helm-next-line
   "C-k" 'helm-previous-line
   "C-h" 'helm-next-source
   "C-l" (kbd "RET")))

(use-package helm-files
  :straight nil
  :after helm
  :config
  (general-define-key
   :keymaps '(helm-find-files-map helm-read-file-map)
   "C-l" 'helm-execute-persistent-action
   "C-h" 'helm-find-files-up-one-level))

(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-base-command "rg --smart-case --no-heading --vimgrep")

  (defun gf/helm-ag-goto ()
    "Go to the occurrence on the current line and recenter."
    (interactive)
    (helm-ag-mode-jump-other-window)
    (recenter))

  (defun gf/helm-ag-show ()
    "Show a compilation in the other window, but stay in the compilation buffer."
    (interactive)
    (gf/helm-ag-goto)
    (other-window -1))

  (general-define-key
   :keymaps 'helm-do-ag-map
   "M-RET" 'helm-ag--run-save-buffer
   "M-e" 'helm-ag-edit)

  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'helm-ag-mode-map
   "q" 'kill-this-buffer)

  (general-define-key
   :states '(normal)
   :keymaps 'helm-ag-mode-map
   "RET" 'gf/helm-ag-show
   "M-RET" 'gf/helm-ag-goto))

(use-package helm-css-scss
  :config
  (setq helm-css-scss-split-with-multiple-windows nil))

(use-package helm-swoop
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-split-direction 'split-window-horizontally
        helm-swoop-use-line-number-face t))

(use-package helm-dash
  :config

  (defvar helm-dash-required-docsets '() "A list of required helm-dash-docsets")

  (when gf/is-mac
    (setq helm-dash-browser-func 'browse-url))

  (setq helm-dash-required-docsets
        '(
          Ansible
          Apache_HTTP_Server
          BackboneJS
          Bash
          CSS
          Font_Awesome
          HTML
          Haskell
          Jade
          JavaScript
          LaTeX
          Markdown
          NodeJS
          PHP
          PHPUnit
          SaltStack
          Symfony
          Twig
          Vagrant
          jQuery
          ))

  (defun gf/helm-dash-install-docsets ()
    "Install required docsets"
    (interactive)
    (dolist (doc (mapcar 'symbol-name helm-dash-required-docsets))
      (when (not (member doc (helm-dash-installed-docsets)))
        (message (format "Installing helm-dash docset '%s'" doc))
        (helm-dash-install-docset doc))))

  (defun gf/helm-dash-upgrade-docsets ()
    "Upgrade installed docsets"
    (interactive)
    (dolist (doc (helm-dash-installed-docsets))
      (message (format "Upgrading helm-dash docset '%s'" doc))
      (helm-dash-update-docset doc)))

  ;; By default, no docsets are enabled.
  (setq helm-dash-common-docsets nil))

;; (defun helm-dash-js ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("JavaScript" "BackboneJS" "jQuery")))
;; (add-hook 'js2-mode-hook 'helm-dash-js)

;; (defun helm-dash-html ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Html" "Font_Awesome")))
;; (add-hook 'html-mode-hook 'helm-dash-html)
;; (add-hook 'web-mode-hook 'helm-dash-html)

;; (defun helm-dash-css ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CSS")))
;; (add-hook 'css-mode-hook 'helm-dash-css)

;; (defun helm-dash-shell ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Bash")))
;; (add-hook 'sh-mode-hook 'helm-dash-shell)

(provide 'setup-core)
