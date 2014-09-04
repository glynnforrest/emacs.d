;; Autocomplete
(define-key ac-complete-mode-map (kbd "TAB") nil)
(define-key ac-complete-mode-map [tab] nil)
(define-key ac-mode-map (kbd "C-<return>" ) 'evil-ret)


;; Insert mode
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-insert-state-map (kbd "C-v") 'evil-paste-after)

(define-key global-map (kbd "C-x e") 'gf/eval-and-replace-sexp)
(define-key global-map (kbd "C-x C-e") 'eval-last-sexp)
(define-key global-map (kbd "C-x E") 'gf/eval-print-last-sexp)

;; Normal mode
(define-key evil-normal-state-map " " 'evil-ex)

(define-key evil-normal-state-map ",a" 'helm-imenu)
(define-key evil-normal-state-map ",B" 'kill-matching-buffers)
(define-key evil-normal-state-map ",C" 'cd)
(define-key evil-normal-state-map ",c" 'gf/comment-or-uncomment-line)
(define-key evil-normal-state-map ",d" 'dired)
(define-key evil-normal-state-map ",D" 'delete-current-buffer-file)
(define-key evil-normal-state-map ",F" 'helm-find-files)
(define-key evil-normal-state-map ",f" 'helm-ls-git-ls)
(define-key evil-normal-state-map ",g" 'magit-status)
(define-key evil-normal-state-map ",G" 'git-gutter:revert-hunk)
(define-key evil-normal-state-map ",I" 'gf/save-and-eval-buffer)
(define-key evil-normal-state-map ",i" 'gf/open-init-file)
(define-key evil-normal-state-map ",m" 'ace-jump-mode)
(define-key evil-normal-state-map ",r" 'helm-recentf)
(define-key evil-normal-state-map ",R" 'rename-current-buffer-file)
(define-key evil-normal-state-map ",S" 'gf/split-window-and-move-below)
(define-key evil-normal-state-map ",s" 'gf/split-window-and-move-right)
(define-key evil-normal-state-map ",u" 'undo-tree-visualize)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",p" 'gf/toggle-switch-to-project-org-file)
(define-key evil-normal-state-map ",q" 'evil-quit)
(define-key evil-normal-state-map ",!" 'flycheck-next-error)
(define-key evil-visual-state-map (kbd ", TAB") 'untabify)

;; Switch gj and j, gk and k
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)

(define-key evil-normal-state-map "gc" 'gf/end-of-sentence-to-comma)
(define-key evil-normal-state-map "gs" 'gf/comma-to-end-of-sentence)
(define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
(define-key evil-normal-state-map "z=" 'ispell-word)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
(define-key evil-normal-state-map (kbd "M-k") 'move-text-up)


(define-key evil-normal-state-map ",T" 'try-code)
(add-hook 'prog-mode-hook (lambda ()
                           (progn)
                           (define-key global-map (kbd "C-t") 'clever-rotate-text)
                           (define-key global-map (kbd "C-T") 'clever-rotate-text-backward)))

;; Visual mode
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map " " 'evil-ex)

;; Narrowing
(define-key evil-visual-state-map ",e" 'eval-region)
(define-key evil-visual-state-map ",n" 'narrow-to-region)
(define-key evil-normal-state-map ",N" 'narrow-to-defun)
(define-key evil-normal-state-map ",n"
  (lambda()
    (interactive)
    (widen)
    (recenter)))


;; General mappings that should work in most modes
;; Windows
(define-key global-map (kbd "C-j") 'evil-window-down)
(define-key global-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-l") 'evil-window-right)
;; get help-map with f1 instead of C-h
(define-key global-map (kbd "C-h") 'evil-window-left)
(define-key global-map (kbd "C-<up>") 'delete-window)
(define-key global-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-normal-state-map ",x" 'kill-this-buffer)
(define-key global-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key global-map (kbd "C-S-<down>") (lambda ()
                                            (interactive)
                                            (kill-this-buffer)
                                            (delete-window)))
(define-key global-map (kbd "M-q") 'gf/quit-other-window)
;; Buffers
;; Change buffers with left and right, Ctrl if not in evil-mode
(define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
(define-key global-map (kbd "C-<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key help-mode-map (kbd "C-<down>") 'kill-this-buffer)
(define-key magit-mode-map (kbd "C-<down>") 'kill-this-buffer)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-'") (lambda()
                                     (interactive)
                                     (text-scale-set 0)))
(define-key global-map (kbd "<f5>" ) 'projectile-invalidate-cache)
(define-key global-map (kbd "M-b") 'helm-mini)
(define-key global-map (kbd "M-B") 'previous-buffer)
(define-key global-map (kbd "<mouse-3>") nil)


(require 'undo-tree)
;; When working in a project, it's useful to have quick access to the
;; project notes, a repl, shell and scratch buffer quickly.
;; M-/ should go to the repl for the current major mode
(define-key global-map (kbd "M-/") 'ielm)
(define-key global-map (kbd "M-?") 'eshell)
(define-key global-map (kbd "C-M-/") 'gf/switch-to-scratch-buffer)

;; Magit
(define-key magit-mode-map (kbd "C-p") 'magit-push)

(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key undo-tree-map (kbd "C-<down>") 'kill-this-buffer)

(define-key global-map (kbd "C-c f") 'fix-double-capital)

(define-key global-map (kbd "C-c u") 'gf/open-url-from-buffer)
(define-key global-map (kbd "C-c U") 'gf/open-recent-url)

(define-key global-map (kbd "C-c e") (lambda ()
                                       (interactive)
                                       (gf/find-file-in-directory "~/.emacs.d")))

(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
;; use M-h to highlight everything

(provide 'mappings)
