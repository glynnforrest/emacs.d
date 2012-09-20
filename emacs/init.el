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
	rainbow-mode
	rainbow-delimiters
	smex
	surround
	test-case-mode
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

;; Load appearance settings
(require 'init-appearance)

;; Load personal configurations, like usernames and passwords
(require 'personal)

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
;; evil
(require 'evil)
(evil-mode 1)
(setq evil-default-cursor t)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
;; Switch gj and j, gk and k
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)

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

;; Start in insert mode / emacs for some modes
(add-to-list 'evil-emacs-state-modes 'package-menu-mode)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'occur-mode 'normal)

;; toggle comments
(define-key evil-visual-state-map ",c" (lambda()
										 (interactive)
										 (comment-or-uncomment-region (region-beginning) (region-end))
										 (evil-visual-restore)))
(define-key evil-normal-state-map ",c" 'comment-or-uncomment-line)

;; browse the kill ring with helm
(require 'helm)
(define-key evil-normal-state-map ",p" 'helm-show-kill-ring)
(setq x-select-enable-clipboard t)

(defun comment-or-uncomment-line ()
  "Comments or uncomments the current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

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
(define-key evil-normal-state-map ",t" 'test-case-run)
(define-key evil-normal-state-map ",T" 'test-case-run-all)

;; yasnippet
(require 'yasnippet)
;; Don't use bundled snippets
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;; don't expand part of words
(setq yas/key-syntaxes '("w_" "w_." "^ "))

(require 'auto-complete-config)
;; yasnippet / auto-complete fix
(defun ac-yasnippet-candidates ()
  (with-no-warnings
 (cond (;; 0.8 onwards
           (fboundp 'yas-active-keys)
           (all-completions ac-prefix (yas-active-keys)))
          (;; >0.6.0
           (fboundp 'yas/get-snippet-tables)
           (apply 'append (mapcar 'ac-yasnippet-candidate-1
                                  (condition-case nil
                                      (yas/get-snippet-tables major-mode)
                                    (wrong-number-of-arguments
                                     (yas/get-snippet-tables)))))
           )
          (t
           (let ((table
                  (if (fboundp 'yas/snippet-table)
                      ;; <0.6.0
                      (yas/snippet-table major-mode)
                    ;; 0.6.0
                    (yas/current-snippet-table))))
             (if table
			(ac-yasnippet-candidate-1 table)))))))

(ac-define-source yasnippet-glynn
  '((depends yasnippet)
	(candidates . ac-yasnippet-candidates)
	(candidate-face . ac-yasnippet-candidate-face)
	(selection-face . ac-yasnippet-selection-face)
	(symbol . "snip")))

(defun ac-config-glynn ()
  (setq-default ac-sources '(
							 ac-source-yasnippet-glynn
							 ac-source-dictionary
							 ac-source-words-in-same-mode-buffers
							 ac-source-filename
							 ))
  (add-hook 'emacs-lisp-mode-hook (lambda()
									(setq ac-sources (append '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols) ac-sources))))
  (add-hook 'css-mode-hook (lambda()
							 (setq ac-sources (append '(ac-source-css-property) ac-sources))))
  (global-auto-complete-mode t)
  (setq ac-auto-start 2))

;; To make a new line instead of accepting suggested word, use C-<return>
(define-key ac-mode-map (kbd "C-<return>" ) 'evil-ret)
(define-key ac-complete-mode-map (kbd "TAB") nil)
(define-key ac-complete-mode-map [tab] nil)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-set-key (kbd "C-SPC") 'auto-complete)
(ac-config-glynn)

(require 'ace-jump-mode)
(define-key evil-normal-state-map ",m" 'ace-jump-mode)

(require 'autopair)
(setq autopair-blink nil)
(autopair-global-mode t)

(recentf-mode 1)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Open recent file: " recentf-list nil t)))
	(when file
	  (find-file file))))

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

(global-linum-mode 1)

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

(define-key global-map (kbd "<mouse-3>") nil)

;; Change buffers with left and right, Ctrl if not in evil-mode
(define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key global-map (kbd "C-<left>") 'previous-buffer)

;; Go to scratch buffer quickly
(define-key global-map (kbd "M-/") (lambda()
									 (interactive)
									 (switch-to-buffer "*scratch*")
									 ))
;; Go to eshell buffer quickly
(define-key global-map (kbd "M-?") (lambda()
									 (interactive)
									 (eshell)
									 ))


(define-key global-map (kbd "C-<up>") 'delete-window)
(define-key global-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key global-map (kbd "C-S-<down>") (lambda ()
											(interactive)
											(kill-this-buffer)
											(delete-window)))
(define-key evil-normal-state-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-insert-state-map (kbd "C-<right>") 'forward-word)
(define-key evil-insert-state-map (kbd "C-<left>") 'backward-word)
(define-key evil-insert-state-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-insert-state-map (kbd "C-/") 'evil-search-forward)
(define-key evil-insert-state-map (kbd "C-?") 'evil-search-backward)
(define-key evil-insert-state-map (kbd "C-v") 'evil-paste-after)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

(require 'help-mode)
(define-key help-mode-map (kbd "C-<down>") 'kill-this-buffer)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-insert-state-map (kbd "C-s") 'save-buffer)

(require 'projectile)
(projectile-global-mode 1)
(setq projectile-enable-caching t)

(define-key global-map (kbd "<f5>" ) 'projectile-invalidate-cache)
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map ",F" 'find-file)

(require 'helm-git)
(define-key evil-normal-state-map ",G" 'helm-git-find-files)
(setq helm-display-function
	  (lambda (buf)
		(split-window-vertically)
		(other-window 1)
		(switch-to-buffer buf)))

(define-key evil-normal-state-map ",e" (lambda()
										 (interactive)
										 (call-interactively 'find-file)
										 ;;(ido-magic-forward-char)
										 ))
(define-key evil-normal-state-map ",r" 'recentf-ido-find-file)
(define-key evil-normal-state-map ",d" 'ido-dired)

;; Occur mode
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
		(if (eq mode major-mode)
			(add-to-list 'buffer-mode-matches buf))))
	buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(define-key evil-normal-state-map ",o" (lambda()
										 (interactive)
										 (call-interactively 'occur)
										 (other-window 1)
										 ))

(define-key evil-normal-state-map ",O" (lambda()
										 (interactive)
										 (call-interactively 'multi-occur-in-this-mode)
										 (other-window 1)
										 ))

(evil-declare-key 'normal occur-mode-map ",e" 'occur-edit-mode)
(evil-declare-key 'normal occur-edit-mode-map ",e" 'occur-cease-edit)

;; Preview occurrences in occur and grep without leaving the buffer
(defun occur-goto-occurrence-recenter ()
  "Go to the occurrence on the current line and recenter."
  (interactive)
  (occur-mode-goto-occurrence)
  (recenter))

(defun occur-display-occurrence-recenter ()
  "Display the occurrence on the current line in another window and recenter."
  (interactive)
  (occur-goto-occurrence-recenter)
  (other-window 1))

(define-key occur-mode-map (kbd "<return>") 'occur-display-occurrence-recenter)
(define-key occur-mode-map (kbd "<S-return>") 'occur-goto-occurrence-recenter)
(evil-declare-key 'normal occur-mode-map (kbd "<return>") 'occur-display-occurrence-recenter)
(evil-declare-key 'normal occur-mode-map (kbd "<S-return>") 'occur-goto-occurrence-recenter)

(defun grep-goto-occurrence-recenter ()
  "Go to the occurrence on the current line and recenter."
  (interactive)
  (compile-goto-error)
  (recenter))

(defun grep-display-occurrence-recenter ()
  "Display the grep result of the current line in another window and recenter."
  (interactive)
  (grep-goto-occurrence-recenter)
  (other-window 1))

(define-key grep-mode-map (kbd "<return>") 'grep-display-occurrence-recenter)
(define-key grep-mode-map (kbd "<S-return>") 'grep-goto-occurrence-recenter)
(evil-declare-key 'normal grep-mode-map (kbd "<return>") 'grep-display-occurrence-recenter)
(evil-declare-key 'normal grep-mode-map (kbd "<S-return>") 'grep-goto-occurrence-recenter)


(define-key evil-normal-state-map ",C" 'cd)


(defun split-window-and-move-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-and-move-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(define-key evil-normal-state-map ",s" 'split-window-and-move-right)
(define-key evil-normal-state-map ",S" 'split-window-and-move-below)
(define-key evil-normal-state-map ",u" 'undo-tree-visualize)
(define-key undo-tree-visualizer-map (kbd "C-<down>") 'kill-this-buffer)

;; Magit
(require 'magit)
(define-key evil-normal-state-map ",g" 'magit-status)
(evil-declare-key 'normal magit-log-edit-mode-map ",w" 'magit-log-edit-commit)
(define-key magit-mode-map "q" (lambda ()
								 (interactive)
								 (if (get-buffer "*magit-process*")
									 (kill-buffer "*magit-process*"))
								 (if (get-buffer "*magit-edit-log*")
									 (kill-buffer "*magit-edit-log*"))
								 (kill-this-buffer)
								 (delete-window)
								 ))
(define-key magit-mode-map (kbd "C-<down>") 'kill-this-buffer)
(define-key magit-mode-map (kbd "C-p") 'magit-push)

;; Preview changes without leaving the buffer
(define-key magit-mode-map (kbd "<S-return>") (lambda()
												(interactive)
											  (let ((current-prefix-arg t))
												(magit-visit-item))))

(define-key magit-mode-map (kbd "<return>") (lambda()
											  (interactive)
											  (universal-argument)
											  (let ((current-prefix-arg t))
												(magit-visit-item))
											  (other-window 1)
											  ))

(setq undo-tree-visualizer-timestamps 1)

(define-key global-map (kbd "M-b") 'ido-switch-buffer)
;; Go back a buffer after picking one
(define-key global-map (kbd "M-B") 'previous-buffer)

(define-key evil-normal-state-map ",b" 'helm-buffers-list)
(define-key evil-normal-state-map ",B" 'kill-matching-buffers)
(define-key evil-normal-state-map " " 'evil-ex)
(define-key evil-visual-state-map " " 'evil-ex)
(define-key evil-visual-state-map ",n" 'narrow-to-region)
(define-key evil-normal-state-map ",n" 'widen)

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(define-key evil-normal-state-map ",i" 'open-init-file)

(defun my-save-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))
(define-key evil-normal-state-map ",I" 'my-save-and-eval-buffer)

(defun my-eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))


(add-hook 'lisp-interaction-mode-hook (lambda()
										(local-unset-key (kbd "C-j"))
										(local-set-key (kbd "M-J") 'my-eval-print-last-sexp)))


(define-key global-map (kbd "C-l") 'evil-window-right)
(define-key global-map (kbd "C-h") 'evil-window-left) ;; get help-map with f1
(define-key global-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-decrease-width)

(defun move-line-up-and-indent ()
  (interactive)
  (transpose-lines 1)
  (evil-previous-line 2)
  (indent-for-tab-command)
  )

(defun move-line-down-and-indent ()
  (interactive)
  (evil-next-line 1)
  (transpose-lines 1)
  (evil-previous-line 1)
  (indent-for-tab-command)
  )

(define-key evil-normal-state-map (kbd "M-j") 'move-line-down-and-indent)
(define-key evil-normal-state-map (kbd "M-k") 'move-line-up-and-indent)
;; nnoremap <A-l> >>
;; nnoremap <A-h> <<


;; Create lines above and below in normal and insert mode with <return>
(define-key evil-normal-state-map (kbd "<S-return>") (lambda()
													   (interactive)
													   (evil-open-below 1)
													   (evil-normal-state 1)))
(define-key evil-normal-state-map (kbd "<C-S-return>") (lambda()
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
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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

(require 'init-multiple-cursors)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(define-key evil-visual-state-map "mb" 'evil-mc-edit-beginnings-of-lines)
(define-key evil-visual-state-map "me" 'evil-mc-edit-ends-of-lines)
(define-key evil-visual-state-map "mm" 'evil-mc-switch-to-cursors)

(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
		 (current-buffer)))

(define-key evil-normal-state-map ",E" 'eval-and-replace-sexp)
(define-key evil-visual-state-map ",e" 'eval-region)

;; Multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
				  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
				  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "php4" "php5"))
(define-key evil-normal-state-map ",q" 'multi-web-mode)

(require 'php-mode)

(add-hook 'php-mode-hook (lambda()
						   (setup-electric-semicolon php-mode-map)))

(defun setup-electric-semicolon (mode-map)
  "Adds mappings for electric semicolon to MODE-MAP.
Press ; for electric-semicolon, C-; to insert a semicolon."
  (evil-declare-key 'insert mode-map ";" 'electric-semicolon)
  (evil-declare-key 'insert mode-map (kbd "C-;") (lambda()
												   (interactive)
												   (insert ";"))))

(defun electric-semicolon ()
  "Inserts a semicolon at the end of the current line if not already there."
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")))

(defun file-has-doctype ()
  (if (string= (upcase (buffer-substring-no-properties 1 10)) "<!DOCTYPE") t nil))

;; enable multi-web-mode only for php files that begin with a doctype
(add-hook 'php-mode-hook (lambda()
						   (if (file-has-doctype) (multi-web-mode))))
(add-hook 'html-mode-hook 'multi-web-mode)

(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun buffer-file-name-body ()
  (if (buffer-file-name)
	  (first (split-string (file-name-nondirectory (buffer-file-name)) "\\.")))
  )

(require 'init-org)

;; Dired mode
(require 'dired)
(require 'dired+)
(add-hook 'dired-mode-hook (lambda ()
							 (interactive)
							 (rename-buffer "*Dired*")
							 ))

;; Up directory fix
(defadvice dired-up-directory (around dired-up-fix activate)
  (interactive)
  (rename-buffer "*Dired-old*")
  ad-do-it
  (previous-buffer)
  (kill-this-buffer)
  )

(evil-declare-key 'normal dired-mode-map ",e" (lambda ()
												(interactive)
												(dired-toggle-read-only)
												(evil-normal-state)
												(evil-forward-char)
												))
(evil-declare-key 'normal dired-mode-map "\\" 'dired-up-directory)
(evil-declare-key 'normal dired-mode-map "q" 'evil-record-macro)
(evil-declare-key 'normal wdired-mode-map ",e" 'wdired-finish-edit)
(evil-declare-key 'normal wdired-mode-map ",a" 'wdired-abort-changes)
(define-key dired-mode-map (kbd "M-b") 'ido-switch-buffer)
(define-key dired-mode-map (kbd "C-h") 'evil-window-left)
(toggle-diredp-find-file-reuse-dir 1)

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
							   (modify-frame-parameters nil `((alpha . 100)))))

(define-key global-map (kbd "<f9>") 'toggle-writeroom)

;; ERC
(require 'erc)
;; my-erc-nick should be in personal.el
(setq erc-nick my-erc-nick)
(add-hook 'erc-mode-hook (lambda ()
						   (interactive)
						   (linum-mode -1)))

;; eshell
(require 'eshell)
(evil-declare-key 'normal eshell-mode-map "i" (lambda ()
												(interactive)
												(evil-goto-line)
												(evil-append-line 1)
												))
(evil-declare-key 'normal eshell-mode-map (kbd "C-j") 'evil-window-down)
(evil-declare-key 'insert eshell-mode-map (kbd "C-j") 'evil-window-down)
(evil-declare-key 'normal eshell-mode-map (kbd "C-<up>") 'delete-window)

;; node REPL
(require 'js-comint)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 node")
