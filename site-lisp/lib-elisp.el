;; pp-macroexpand-last-sexp does the same, but isn't read-only and writes a noisy message
(defun gf/show-previous-sexp-macro-expand ()
  "Show the result of macro expanding the previous sexp."
  (interactive)
  (let ((lisp (macroexpand (elisp--preceding-sexp))))
  (with-current-buffer-window
   "*Macro Expansion*" nil nil
   (pp lisp)
   (emacs-lisp-mode)
   (read-only-mode))))

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

(provide 'lib-elisp)
