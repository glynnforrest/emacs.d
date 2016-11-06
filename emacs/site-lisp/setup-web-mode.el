(use-package web-mode :ensure t
  :mode
  (("\\.twig\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.hbs\\'" . web-mode))
  :config
  (setq
   web-mode-disable-auto-pairing nil
   web-mode-disable-css-colorization nil
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 4
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


(provide 'setup-web-mode)
