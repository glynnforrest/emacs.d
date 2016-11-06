(use-package zenburn-theme :ensure t
  :config
  (load-theme 'zenburn t))


(defun gf/trim-ui ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode fringe-mode))
    (when (fboundp mode) (funcall mode -1))))

(gf/trim-ui)
(add-hook 'after-make-frame-functions 'gf/trim-ui)

(blink-cursor-mode -1)
(setq line-spacing 0)

(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'setup-appearance)
