;set up packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages '(evil
                            molokai-theme
                            auto-complete
                            php-mode
                            projectile
                            rainbow-delimiters
                            smex
                            zencoding-mode
                            magit
                            js2-mode)
  "A list of required packages for this configuration.")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'molokai-theme)

(setq hl-line-sticky-flag 1)
(global-hl-line-mode t)

(require 'evil)  
(evil-mode 1)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'projectile)
(projectile-global-mode 1)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(recentf-mode 1)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(ido-mode 1)
(setq ido-enable-flex-matching t)

;Prevent Emacs from auto-changing the working directory
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
;(menu-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

;Change buffers with left and right, Ctrl if not in evil-mode
(define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key global-map (kbd "C-<left>") 'previous-buffer)
(define-key global-map (kbd "C-<up>") 'delete-window)
(define-key global-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-insert-state-map (kbd "C-<right>") 'forward-word)
(define-key evil-insert-state-map (kbd "C-<left>") 'backward-word)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map ",F" 'find-file)
(define-key evil-normal-state-map ",r" 'recentf-ido-find-file)
(define-key evil-normal-state-map ",s" 'split-window-right)
(define-key evil-normal-state-map ",S" 'split-window-below)
(define-key evil-normal-state-map ",u" 'undo-tree-visualize)
(setq undo-tree-visualizer-timestamps 1)

(define-key global-map (kbd "M-b") 'ido-switch-buffer)
(define-key evil-normal-state-map " " 'evil-ex)


(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(define-key evil-normal-state-map ",i" 'open-init-file)

(defun my-save-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))
(define-key evil-normal-state-map ",e" 'my-save-and-eval-buffer)

(defun remap-lisp-c-j ()
  (local-unset-key (kbd "C-j"))
  (local-set-key (kbd "M-J") 'my-eval-print-last-sexp))

(defun my-eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))

 
(define-key global-map (kbd "C-l") 'evil-window-right)
(define-key global-map (kbd "C-h") 'evil-window-left) ;get help-map with f1
(define-key global-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-j") 'evil-window-down)
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
;nnoremap <A-l> >>
;nnoremap <A-h> <<

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

;universal escape key as well as C-g
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;Cycle the color of the cursor
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'. 
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )
