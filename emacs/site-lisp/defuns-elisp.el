(defun gf/show-previous-sexp-macro-expand ()
  "Show the result of macro expanding the previous sexp."
  (interactive)
  (let ((lisp (macroexpand (elisp--preceding-sexp))))
  (with-current-buffer-window
   "*Macro Expansion*" nil nil
   (pp lisp)
   (emacs-lisp-mode)
   (read-only-mode))))

(provide 'defuns-elisp)
