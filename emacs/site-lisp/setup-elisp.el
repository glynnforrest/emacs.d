(require 'paredit)
(require 'el-autoyas)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")

;; Set up some useful modes and set spaces
(defun setup-elisp ()
  (eldoc-mode t)
  (paredit-mode t)
  (smartparens-mode -1)
  (setq indent-tabs-mode nil)
  (el-autoyas-enable)
  (elisp-slime-nav-mode t)
  )

(add-hook 'emacs-lisp-mode-hook 'setup-elisp)
(add-hook 'lisp-interaction-mode-hook 'setup-elisp)
(add-hook 'ielm-mode-hook 'setup-elisp)

(add-hook 'lisp-interaction-mode-hook (lambda()
                                        (local-set-key (kbd "C-S-RET") 'gf/eval-print-last-sexp)))


(defun gf/eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(defun lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
          (interactive)
          (let (sym)
            ;; sigh, function-at-point is too clever.  we want only the first half.
            (cond ((setq sym (ignore-errors
                               (with-syntax-table emacs-lisp-mode-syntax-table
                                 (save-excursion
                                   (or (not (zerop (skip-syntax-backward "_w")))
                                       (eq (char-syntax (char-after (point))) ?w)
                                       (eq (char-syntax (char-after (point))) ?_)
                                       (forward-sexp -1))
                                   (skip-chars-forward "`'")
                                   (let ((obj (read (current-buffer))))
                                     (and (symbolp obj) (fboundp obj) obj))))))
                   (describe-function sym))
                  ((setq sym (variable-at-point)) (describe-variable sym)))))

(define-key lisp-mode-shared-map (kbd "M-RET") 'lisp-describe-thing-at-point)

(add-hook 'ielm-mode-hook (lambda()
                            (define-key ielm-map (kbd "M-RET") 'lisp-describe-thing-at-point)))

(defun clever-splice-sexp-killing-backward ()
  "Wrapper around paredit-splice-sexp-killing-backward that moves the whole symbol."
  (interactive)
  (evil-backward-word-begin)
  (paredit-splice-sexp-killing-backward))

(defun clever-splice-sexp-killing-forward ()
  "Wrapper around paredit-splice-sexp-killing-forward that moves the whole symbol."
  (interactive)
  (evil-forward-word-begin)
  (paredit-splice-sexp-killing-forward))

(provide 'setup-elisp)
