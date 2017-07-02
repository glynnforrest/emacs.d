(use-package css-mode :ensure t
  :mode ("\\.css\\'" . css-mode)
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

  (general-define-key
   :states '(normal visual)
   :keymaps 'css-mode-map
   "S" 'gf/css-change-value)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'css-mode-map
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   "e" 'skewer-css-eval-current-rule)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'css-mode-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "a" 'helm-css-scss))

(use-package scss-mode :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (setq css-indent-offset 2))

(use-package skewer-mode :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook 'skewer-css-mode))

(provide 'setup-css)
