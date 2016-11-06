(use-package magit :ensure t
  :config
  (setq magit-log-arguments '("-n256" "--decorate"))
  (setq inhibit-magit-revert t))

(use-package git-gutter :ensure t
   :config
   (global-git-gutter-mode t))

(use-package git-timemachine :ensure t
  :config
  (defhydra hydra-git-timemachine ()
    "Git timemachine"
    ("n" git-timemachine-show-next-revision "Next commit")
    ("p" git-timemachine-show-previous-revision "Previous commit")
    ("q" git-timemachine-quit "Quit"))

  (defun gf/git-timemachine ()
    "Start git-timemachine with a hydra."
    (interactive)
    (git-timemachine)
    (hydra-git-timemachine/body)))

(provide 'setup-git)
