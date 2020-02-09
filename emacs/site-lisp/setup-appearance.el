(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t))


(defun gf/trim-ui ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode fringe-mode))
    (when (fboundp mode) (funcall mode -1))))

(gf/trim-ui)
(add-hook 'after-make-frame-functions (lambda(frame)
                                        (gf/trim-ui)))

(blink-cursor-mode -1)
(setq line-spacing 0)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :init
  (mapc (lambda (hook)
          (add-hook hook 'rainbow-mode))
        '(css-mode-hook
          emacs-lisp-mode-hook
          haskell-mode-hook))
  :config
  (diminish 'rainbow-mode))

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

(provide 'setup-appearance)
