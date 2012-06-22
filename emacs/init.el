;; set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages '(
                            ace-jump-mode
                            auto-complete
                            dired+
                            evil
                            js2-mode
                            magit
                            molokai-theme
                            multi-web-mode
                            org
                            php-mode
                            projectile
                            rainbow-delimiters
                            smex
                            surround
                            zencoding-mode
                            )
  "A list of required packages for this setup.")

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
;; Load personal configurations, like usernames and passwords
(require 'personal)

(require 'evil)
(evil-mode 1)
(setq evil-default-cursor t)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)

;(require 'molokai-theme)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-bright)

(setq hl-line-sticky-flag 1)
(global-hl-line-mode t)

(global-surround-mode t)
(global-auto-revert-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'projectile)
(projectile-global-mode 1)


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'ace-jump-mode)
(define-key evil-normal-state-map ",m" 'ace-jump-mode)

(require 'php-mode)
(electric-pair-mode t)

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
(setq ido-max-directory-size 100000)

;; Prevent Emacs from auto-changing the working directory
(defun find-file-keep-directory ()
  (interactive)
  (setq saved-default-directory default-directory)
  (ido-find-file)
  (setq default-directory saved-default-directory))


(global-linum-mode 1)


(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

;; Change buffers with left and right, Ctrl if not in evil-mode
(define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key global-map (kbd "C-<left>") 'previous-buffer)
(define-key global-map (kbd "C-<up>") 'delete-window)
(define-key global-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key global-map (kbd "C-M-<up>") (lambda ()
                                          (interactive)
                                          (kill-this-buffer)
                                          (delete-window)))
(define-key evil-normal-state-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-insert-state-map (kbd "C-<right>") 'forward-word)
(define-key evil-insert-state-map (kbd "C-<left>") 'backward-word)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map ",F" 'find-file)
(define-key evil-normal-state-map ",r" 'recentf-ido-find-file)
(define-key evil-normal-state-map ",d" 'ido-dired)
(define-key evil-normal-state-map ",cd" 'cd)

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

;Magit
(require 'magit)
(define-key evil-normal-state-map ",g" 'magit-status)
(evil-declare-key 'normal magit-log-edit-mode-map ",w" 'magit-log-edit-commit)
(define-key magit-mode-map "q" (lambda ()
                                 (interactive)
                                 (if (get-buffer "*magit-process*")
                                     (kill-buffer "*magit-process*"))
                                 (kill-this-buffer)
                                 ))

(setq undo-tree-visualizer-timestamps 1)

(define-key global-map (kbd "M-b") 'ido-switch-buffer)
(define-key evil-normal-state-map " " 'evil-ex)
(define-key evil-visual-state-map " " 'evil-ex)
(define-key evil-visual-state-map "n" 'narrow-to-region)
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

(defun remap-lisp-c-j ()
  (local-unset-key (kbd "C-j"))
  (local-set-key (kbd "M-J") 'my-eval-print-last-sexp))

(defun my-eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))


(define-key global-map (kbd "C-l") 'evil-window-right)
(define-key global-map (kbd "C-h") 'evil-window-left) ;; get help-map with f1
(define-key global-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-decrease-width)

(add-hook 'lisp-interaction-mode-hook 'remap-lisp-c-j)

(defun my-move-line-up-and-indent ()
  (interactive)
  (transpose-lines 1)
  (evil-previous-line 2))

(defun my-move-line-down-and-indent ()
  (interactive)
  (evil-next-line 1)
  (transpose-lines 1)
  (evil-previous-line 1))

(define-key evil-normal-state-map (kbd "M-j") 'my-move-line-down-and-indent)
(define-key evil-normal-state-map (kbd "M-k") 'my-move-line-up-and-indent)
;; nnoremap <A-l> >>
;; nnoremap <A-h> <<

(define-key evil-normal-state-map (kbd "<S-return>") 'my-evil-new-line)
(defun my-evil-new-line ()
  (interactive)
  (evil-open-below 1)
  (evil-normal-state 1))

(define-key evil-normal-state-map (kbd "<C-S-return>") 'my-evil-new-line-above)
(defun my-evil-new-line-above ()
  (interactive)
  (evil-open-above 1)
  (evil-normal-state 1))

;; universal escape key as well as C-g
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; Cursor configuration.
(blink-cursor-mode -1)
(setq evil-insert-state-cursor '("#ffffff" bar))
(setq evil-normal-state-cursor '("#ffffff" box))
(setq evil-emacs-state-cursor '("#d72626" bar))

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

(define-key evil-normal-state-map ",er" 'eval-and-replace-sexp)

;; Multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'php-mode)
(setq mweb-tags '((php-mode "\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (html-mode "<.+>" "</.+>")
                  (html-mode "\\?>" "<\\?")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "php4" "php5"))
(multi-web-global-mode 1)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Org mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(mapcar (lambda (state)
    (evil-declare-key state org-mode-map
      (kbd "M-l") 'org-metaright
      (kbd "M-h") 'org-metaleft
      (kbd "M-k") 'org-metaup
      (kbd "M-j") 'org-metadown
      (kbd "M-L") 'org-shiftmetaright
      (kbd "M-H") 'org-shiftmetaleft
      (kbd "M-K") 'org-shiftmetaup
      (kbd "M-J") 'org-shiftmetadown))
  '(normal insert))

(evil-declare-key 'normal org-mode-map ",t" 'org-todo)
;; Dired mode
(require 'dired)
(require 'dired+)
(add-hook 'dired-mode-hook (lambda ()
                             (interactive)
                             (rename-buffer "*Dired*")
                             ))

;;Up directory fix
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
(evil-declare-key 'normal wdired-mode-map ",e" 'wdired-finish-edit)
(evil-declare-key 'normal wdired-mode-map ",a" 'wdired-abort-changes)
(define-key dired-mode-map (kbd "M-b") 'ido-switch-buffer)
(toggle-diredp-find-file-reuse-dir 1)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;;ERC
(require 'erc)
;;my-erc-nick should be in personal.el
(setq erc-nick my-erc-nick)
(add-hook 'erc-mode-hook (lambda () 
                 (interactive)
                 (linum-mode -1)))

;;Terminal
(define-key evil-normal-state-map ",x" (lambda ()
                                         (interactive)
                                         (ansi-term "/bin/bash")
                                         ))
;;todo: override M-b for buffer switching.
