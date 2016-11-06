(use-package css-mode :ensure t
  :config
  (setq css-indent-offset 2)

  (defun gf/css-change-to-px ()
    "Change text until `px` in css mode."
    (interactive)
    (if (looking-at-p " ")
	(evil-forward-word-begin))
    (let (( beg (point)))
      (evil-find-char 1 (string-to-char "p"))
      (evil-change beg (point))))

  (use-package skewer-mode :ensure t
    :config
    (add-hook 'css-mode-hook 'skewer-css-mode)))


(provide 'setup-css)
