(eval-when-compile (require 'use-package))

(require 'defuns-core)

(use-package general)
(defvar gf/leader-key "SPC"
  "Leader key prefix to use for key bindings.")

(defvar gf/non-normal-leader-key "M-SPC"
  "Leader key prefix to use for key bindings in non-normal evil modes.")

(defvar gf/major-mode-leader-key "SPC m"
  "Leader key prefix to use for major mode key bindings.")

(defvar gf/major-mode-non-normal-leader-key "M-SPC m"
  "Leader key prefix to use for major mode key bindings in non-normal evil modes.")

(defmacro gf/key (cmd &rest args)
  "Helper macro to add an argument to a key binding.

(general-define-key
  \"a\" (gf/key 'command-name \"arg1\" \"arg2\"))

or if using plists

(general-define-key
  \"a\" `(,(gf/key 'command-name \"arg1\" \"arg2\") :which-key \"run command-name\"))
"
  `(lambda () (interactive) ,(append (cdr cmd) args)))

(use-package diminish)

(use-package which-key
  :diminish ""
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode t))

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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish ""
  :hook (css-mode emacs-lisp-mode haskell-mode))

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
 undo-tree-auto-save-history nil
 vc-follow-symlinks t)

;; https://stackoverflow.com/a/2903256
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; enable normally disabled functions
(put 'narrow-to-region 'disabled nil)

(use-package restart-emacs)

(use-package s)

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :diminish "")

(use-package evil
  :init
  (setq-default
   evil-want-keybinding nil
   evil-want-integration t
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t
   evil-want-C-w-delete nil
   evil-want-C-w-in-emacs-state t
   evil-undo-system 'undo-tree)

  :config
  (evil-mode 1)

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
    (evil-visual-restore)))

(use-package evil-numbers
  :after evil)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t))

;; To not miss surround mode
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

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

(use-package evil-nerd-commenter
  :defer t
  :commands (evilnc-comment-operator)
  :init
  (general-define-key
   :states '(normal visual)
   "gc" 'evilnc-comment-operator))

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
  (projectile-mode)
  (require 'defuns-projects))

(use-package vertico
  :straight (vertico :includes vertico-directory
                     :files (:defaults "extensions/vertico-directory.el"))
  :init
  (vertico-mode)

  :config
  (general-define-key
   :keymaps 'vertico-map
   "M-RET" 'embark-export
   "C-l" 'vertico-insert
   "C-j" 'vertico-next
   "M-j" 'vertico-next-group
   "C-k" 'vertico-previous
   "M-k" 'vertico-previous-group)

  (setq vertico-cycle t))

(use-package vertico-directory
  :config
  (general-define-key :keymaps 'vertico-map
                      "C-l" 'vertico-directory-enter
                      "C-h" 'vertico-directory-delete-word)

  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :config
  (setq consult-preview-key nil
        consult-project-root-function #'projectile-project-root
        ))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark)

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (general-define-key
   :keymaps 'grep-mode-map
   "M-e" 'wgrep-change-to-wgrep-mode))

(use-package company
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-limit 10)

  (general-define-key
   :keymaps '(company-active-map company-search-map company-filter-map)
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "TAB" nil))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-imenu nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-snippet nil
        ;; lsp-imenu-index-function 'lsp-imenu-create-categorized-index
        ;; lsp-imenu-index-symbol-kinds '(Function Method)
        lsp-intelephense-multi-root t
        lsp-symbol-highlighting-skip-current t)
  :hook ((php-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]var/cache\\'")
  )

(setq gc-cons-threshold 100000000)

(add-hook 'find-file-hook 'gf/maybe-smerge)

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(use-package move-text)

(use-package smartparens
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :hook ((prog-mode comint-mode) . smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)))

(use-package rotate-text
  :commands (gf/clever-rotate-text gf/clever-rotate-text-backward)
  :init
  (defhydra hydra-rotate-text ()
    "Rotate text"
    ("n" gf/clever-rotate-text "Next")
    ("p" gf/clever-rotate-text-backward "Previous")
    ("q" nil "Quit"))

  :config
  (defun gf/clever-rotate-text ()
    "Wrapper to rotate-text that will try the start of the line as well
as the current word."
    (interactive)
    (if (not (condition-case nil
                 (rotate-text 1)
               (error nil)))
        (save-excursion
          (evil-first-non-blank)
          (rotate-text 1))))

  (defun gf/clever-rotate-text-backward ()
    "Wrapper to rotate-text-backward that will try the start of the
line as well as the current word."
    (interactive)
    (if (not (condition-case nil
                 (rotate-text-backward 1)
               (error nil)))
        (save-excursion
          (evil-first-non-blank)
          (rotate-text-backward 1)))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; Don't use bundled snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; don't expand part of words
  (setq yas-key-syntaxes '("w_" "w_." "^ "))
  (yas-global-mode 1)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'snippet-mode-map
   "t" 'yas-tryout-snippet))

(use-package ws-butler
  :config
  ;; Use spaces by default, override in individual modes with hooks.
  ;; The general rule of thumb is 4 spaces, with some mode-specific
  ;; exceptions.
  (setq-default
   c-basic-offset 4
   tab-width 4
   indent-tabs-mode nil)

  ;; Final newline is important
  ;; http://robots.thoughtbot.com/no-newline-at-end-of-file
  (setq require-final-newline t)
  (setq mode-require-final-newline t)
  (ws-butler-global-mode t))

(use-package editorconfig
  :diminish ""
  :config
  (editorconfig-mode 1)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(defun gf/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun gf/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun gf/indent-cleanup-buffer ()
  "Indent and cleanup the whitespace of the entire buffer."
  (interactive)
  (gf/indent-buffer)
  (ws-butler-clean-region (point-min) (point-max))
  (gf/untabify-buffer))

;; Change to unix line endings when loading a DOS file
;; http://www.emacswiki.org/emacs/DosToUnix
(defun gf/dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(use-package try-code
  :defer t
  :commands try-code)

(use-package ace-link)

(use-package flycheck
  :diminish ""
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             php-phpmd
                                             php-phpcs
                                             scss)

                ;; so flycheck can check (require) calls properly.
                flycheck-emacs-lisp-load-path 'inherit)

  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-standard-error-navigation nil
        flycheck-highlighting-mode 'lines)

  (set-face-attribute 'flycheck-error nil
                      :foreground "#ffffff"
                      :background "#671232"
                      :underline nil)

  (set-face-attribute 'flycheck-warning nil
                      :foreground "#ceb4e2"
                      :background nil
                      :underline nil)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode rst-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  (global-flycheck-mode))

(use-package flyspell
  :diminish "spell"
  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode with-editor-mode rst-mode). flyspell-mode))
  :config
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :after (flyspell)
  :config
  (general-define-key
   :states '(normal)
   "z=" 'flyspell-correct-wrapper))

(use-package emmet-mode
  :hook (html-mode web-mode css-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding t))

(provide 'setup-core)
