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
	auto-complete
	autopair
	browse-kill-ring
	color-theme
	color-theme-monokai
	dired+
	elscreen
	eproject
	evil
	helm
	helm-git
	js2-mode
	js-comint
	magit
	markdown-mode
	multi-web-mode
	org
	php-mode
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

(require 'modes)

;; Share emacs
(require 'server)
(unless (server-running-p)
  (server-start))

;;Allows launching from chrome textareas
(require 'edit-server nil t)
(unless (process-status "edit-server")
  (setq edit-server-new-frame t)
  (edit-server-start))

(setq ido-default-buffer-method 'selected-window)
(require 'setup-evil)
;; toggle comments
(define-key evil-visual-state-map ",c" (lambda()
										 (interactive)
										 (comment-or-uncomment-region (region-beginning) (region-end))
										 (evil-visual-restore)))

;; browse the kill ring with helm
(require 'helm)
(setq x-select-enable-clipboard t)


(setq hl-line-sticky-flag 1)
(global-hl-line-mode t)

(require 'surround)
(global-surround-mode t)
(global-auto-revert-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'test-case-mode)
(define-key evil-normal-state-map ",t" (lambda()
										 (interactive)
										 (save-buffer)
										 (test-case-run)))

;; yasnippet
(require 'yasnippet)
;; Don't use bundled snippets
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;; don't expand part of words
(setq yas/key-syntaxes '("w_" "w_." "^ "))


(require 'setup-autocomplete)

(require 'ace-jump-mode)

(require 'autopair)
(setq autopair-blink nil)
(autopair-global-mode t)

(recentf-mode 1)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
	  (remq 'process-kill-buffer-query-function
			kill-buffer-query-functions))
(setq ido-ignore-buffers (append '("^\*Completions\*" "^\*Help\*" "^\*magit-process\*" "^\*Compile-Log\*" "^\*vc-diff\*") ido-ignore-buffers))
(setq ido-auto-merge-delay-time 99999)
(add-hook 'ido-setup-hook (lambda ()
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

;; Prevent Emacs from auto-changing the working directory
(defun find-file-keep-directory ()
  (interactive)
  (setq saved-default-directory default-directory)
  (ido-find-file)
  (setq default-directory saved-default-directory))

;; Automatically create directories when creating a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
	(let ((dir (file-name-directory filename)))
	  (unless (file-exists-p dir)
		(make-directory dir)))))

(setq make-backup-files nil)
(setq auto-save-default nil)
;; Use tabs
(setq-default c-basic-offset 4
			tab-width 4
			indent-tabs-mode t)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(show-paren-mode t)
(column-number-mode t)
(tooltip-mode -1)


;; Change buffers with left and right, Ctrl if not in evil-mode



;; Go to eshell buffer quickly
(define-key global-map (kbd "M-?") (lambda()
									 (interactive)
									 (eshell)
									 ))



;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

(require 'help-mode)
(require 'helm-git)
(setq helm-display-function
	  (lambda (buf)
		(split-window-vertically)
		(other-window 1)
		(switch-to-buffer buf)))

(setq undo-tree-visualizer-timestamps 1)

;; Go back a buffer after picking one

(evil-declare-key 'normal org-mode-map ",N" 'org-narrow-to-subtree)
(define-key evil-normal-state-map ",n" (lambda()
										 (interactive)
										 (widen)
										 (recenter)))


(define-key global-map (kbd "C-M-e") (lambda ()
									   (interactive)
									   (gf-find-file-in-directory "~/.emacs.d"
																  "Find in emacs folder: ")))



(add-hook 'lisp-interaction-mode-hook (lambda()
										(local-unset-key (kbd "C-j"))
										(local-set-key (kbd "M-J") 'my-eval-print-last-sexp)))



(define-key global-map (kbd "C-'") (lambda()
									 (interactive)
									 (text-scale-set 0)))


;; nnoremap <A-l> >>
;; nnoremap <A-h> <<


;; Create lines above and below in normal and insert mode with <return>
(define-key evil-normal-state-map (kbd "S-<return>") (lambda()
													   (interactive)
													   (evil-open-below 1)
													   (evil-normal-state 1)))
(define-key evil-normal-state-map (kbd "C-S-<return>") (lambda()
														 (interactive)
														 (evil-open-above 1)
														 (evil-normal-state 1)))
(define-key evil-insert-state-map (kbd "S-<return>") (lambda()
													   (interactive)
													   (evil-open-below 1)))
(define-key evil-insert-state-map (kbd "C-S-<return>") (lambda()
														 (interactive)
														 (evil-open-above 1)))

;; universal escape key as well as C-g

;; Remove any trailing whitespace on buffer write
(define-minor-mode remove-trailing-whitespace-mode
  "Toggle remove trailing whitespace on save.
When enabled trailing whitespace is removed before saving."
  :init-value nil
  :global t
  :lighter " W"

  (if remove-trailing-whitespace-mode
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)
	(remove-hook 'before-save-hook 'delete-trailing-whitespace)))

(remove-trailing-whitespace-mode t)

(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

(require 'setup-multiple-cursors)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)


(defun file-has-doctype ()
  (if (string= (upcase (buffer-substring-no-properties 1 10)) "<!DOCTYPE") t nil))

;; enable web-mode for php files that begin with a doctype
(add-hook 'php-mode-hook (lambda()
						   (if (file-has-doctype) (web-mode))))

(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; for yasnippet
(defun buffer-file-name-body ()
  (if (buffer-file-name)
	  (first (split-string (file-name-nondirectory (buffer-file-name)) "\\.")))
  )

(require 'setup-org)
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
							   (modify-frame-parameters nil `((alpha . 100)))))


;; Flyspell
(setq flyspell-issue-message-flag nil)

(dolist (hook '(org-mode-hook magit-log-edit-mode-hook))
  (add-hook hook (lambda ()
				   (flyspell-mode 1)
				   (auto-fill-mode 1)
				   )))

(require 'elscreen)
(elscreen-start)

(require 'setup-eshell)

;; Load various customisations
(require 'appearance)
(require 'defuns)
(require 'setup-occur-grep)
(require 'git-gutter)
(global-git-gutter-mode)
(require 'try-code)
(require 'setup-magit)
(require 'setup-projects)
(require 'setup-js)
(require 'setup-css)
(require 'mappings)
(require 'multiple-cursors)
