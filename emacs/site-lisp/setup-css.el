(use-package css-mode :ensure t
  :config
  (setq css-indent-offset 2)

  (defun gf/css-change-value ()
    "Change the value of a css statement."
    (interactive)
    (beginning-of-line)
    (evil-find-char 1 (string-to-char ":"))
    (forward-char)
    (if (looking-at-p " ")
	(evil-forward-word-begin))
    (let (( beg (point)))
      (evil-find-char 1 (string-to-char "p"))
      (evil-change beg (point))))

  (use-package skewer-mode :ensure t
    :config
    (add-hook 'css-mode-hook 'skewer-css-mode))

  (general-define-key
   :states '(normal visual)
   :keymaps 'css-mode-map
   "s" 'gf/css-change-value
   ",s" 'skewer-css-eval-current-rule))


(provide 'setup-css)
