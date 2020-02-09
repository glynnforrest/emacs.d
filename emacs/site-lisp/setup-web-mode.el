(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t
))

;; (defun gf/web-mode-toggle-markup-offset ()
;;   "Switch between 2 and 4 spaces for markup indentation"
;;   (interactive)
;;   (if (eq web-mode-markup-indent-offset 2)
;;       (setq web-mode-markup-indent-offset 4)
;;     (setq web-mode-markup-indent-offset 2))
;;   (message (format "Set markup indendation to %s spaces" web-mode-markup-indent-offset))
;;   (web-mode))

(defun gf/toggle-php-web-mode ()
  "Switch between php-mode and web-mode for the current buffer."
  (interactive)
  (if (equal (symbol-name (buffer-local-value 'major-mode (current-buffer))) "web-mode")
      (php-mode)
    (web-mode)))

(defun gf/web-maybe-activate-lsp ()
  "Maybe activate language server protocol for the current buffer."
  (if (equal (gf/filename-extension (buffer-file-name)) "vue")
      (lsp-vue-mmm-enable)))

(when (not (version< emacs-version "25.1"))
  (use-package lsp-vue
    :after web-mode
    :config
        (add-hook 'web-mode-hook #'gf/web-maybe-activate-lsp)))

(provide 'setup-web-mode)
