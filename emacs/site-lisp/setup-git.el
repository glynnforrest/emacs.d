(use-package magit
  :commands magit-status
  :config
  (setq
   magit-log-arguments '("-n256" "--decorate")
   magit-log-margin '(t "%H:%M %a %d %b %Y" magit-log-margin-width t 18))
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (general-define-key
   :keymaps '(magit-mode-map magit-diff-mode-map)
   "SPC" 'nil)

  (general-define-key
   :states '(normal)
   :keymaps 'magit-blame-mode-map
   "b" 'magit-blame
   "j" 'magit-blame-next-chunk
   "k" 'magit-blame-previous-chunk
   "y" 'magit-blame-copy-hash))

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :diminish ""
  :config
  (global-git-gutter-mode t))

(use-package git-timemachine
  :config
  (defhydra hydra-git-timemachine ()
    "Git timemachine"
    ("n" git-timemachine-show-next-revision "Next commit")
    ("p" git-timemachine-show-previous-revision "Previous commit")
    ("q" git-timemachine-quit "Quit" :exit t))

  (defun gf/git-timemachine ()
    "Start git-timemachine with a hydra."
    (interactive)
    (git-timemachine)
    (hydra-git-timemachine/body)))

(provide 'setup-git)
