(define-key global-map (kbd "C-x e") 'gf/eval-and-replace-sexp)
(define-key global-map (kbd "C-x C-e") 'eval-last-sexp)
(define-key global-map (kbd "C-x E") 'gf/eval-print-last-sexp)

;; Normal mode

(evil-leader/set-key
  "e" 'gf/maybe-eval-region
  "n" 'gf/narrow-or-widen
  "N" 'narrow-to-defun
  "S" 'gf/split-window-and-move-below
  "s" 'gf/split-window-and-move-right
  )

(defun gf/narrow-or-widen ()
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end))
    (progn
      (widen)
      (recenter))))

(defun gf/maybe-eval-region ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))))

(define-key evil-normal-state-map "gc" 'gf/end-of-sentence-to-comma)
(define-key evil-normal-state-map "gs" 'gf/comma-to-end-of-sentence)
(define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
(define-key evil-normal-state-map "z=" 'ispell-word)

(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-increase-width)


;; Visual mode
(define-key evil-visual-state-map [escape] 'keyboard-quit)

;; When working in a project, it's useful to have quick access to the
;; project notes, a repl, shell and scratch buffer quickly.
;; M-/ should go to the repl for the current major mode
(define-key global-map (kbd "M-/") 'ielm)
(define-key global-map (kbd "M-?") 'eshell)
(define-key global-map (kbd "C-M-/") 'gf/switch-to-scratch-buffer)

(define-key global-map (kbd "C-c f") 'fix-double-capital)

(define-key global-map (kbd "C-c u") 'gf/open-url-from-buffer)
(define-key global-map (kbd "C-c U") 'gf/open-recent-url)

;; use M-h to highlight everything

;; Lisp
(evil-declare-key 'normal lisp-mode-shared-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c C-e") 'gf/eval-and-replace-sexp)

(provide 'mappings)
