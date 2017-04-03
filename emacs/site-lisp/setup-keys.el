(use-package general :ensure t
  :config
  (setq
   gf/major-mode-leader-key "SPC m"
   gf/major-mode-non-normal-leader-key "M-m")

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "a" 'helm-imenu

   "b" '(:ignore t :which-key "buffers")
   "bb" 'helm-mini
   "bd" 'kill-this-buffer
   "bD" 'kill-matching-buffers

   "d" 'helm-dash
   "D" 'helm-dash-at-point

   "e" '(:ignore t :which-key "emacs/eval")
   "eb" 'eval-buffer

   "f" '(:ignore t :which-key "files")
   "fD" 'delete-current-buffer-file
   "fe" 'gf/find-emacs-d-file
   "ff" 'helm-find-files
   "fi" '(gf/open-init-file :which-key "open init.el")
   "fI" '(gf/open-personal-file :which-key "open setup-personal.el")
   "fo" 'gf/find-notes-file
   "fp" 'projectile-find-file
   "fr" 'helm-recentf
   "fR" 'rename-current-buffer-file
   "fs" 'save-buffer
   "fz" '(gf/open-zshrc :which-key "open .zshrc")
   "fZ" '(gf/open-zshrc-local :which-key "open .zshrc.local")

   "g" '(:ignore t :which-key "git")
   "gb" 'magit-blame
   "gs" 'magit-status
   "gt" '(gf/git-timemachine :which-key "git-timemachine")

   "G" 'git-gutter:revert-hunk

   "l" 'evil-lisp-state

   "m" '(:ignore t :which-key "major-mode")

   "o" '(:ignore t :which-key "org")
   "oc" 'org-capture
   "oi" '(gf/find-org-info-file :which-key "open info file")
   "oN" 'gf/commit-notes
   "om" 'gf/find-current-month-notes-file
   "op" 'gf/toggle-switch-to-project-org-file
   "os" 'org-search-view

   "p" '(:ignore t :which-key "projects")
   "pk" 'projectile-kill-buffers
   "ps" 'projectile-switch-project
   "pr" 'projectile-replace

   "P" 'helm-show-kill-ring

   "q" '(:ignore t :which-key "quitting")
   "qf" 'delete-frame
   "qq" 'save-buffers-kill-emacs
   "qQ" 'gf/save-buffers-kill-emacs-no-prompt
   "qr" 'restart-emacs

   "r" 'gf/tmux-run-last
   "R" 'gf/tmux-run

   "s" '(:ignore t :which-key "search")
   "sp" 'counsel-ag

   "t" '(:ignore t :which-key "toggle")
   "tr" '(rainbow-mode)
   "tg" '(global-git-gutter-mode :which-key "git-gutter")
   "tw" '(global-whitespace-mode :which-key "whitespace")
   "tp" '(smartparens-mode :which-key "smartparens")
   "tW" '(ws-butler-mode :which-key "whitespace butler")

   "T" 'try-code

   "u" 'universal-argument
   "U" 'undo-tree-visualize

   "v" '(er/expand-region :which-key "expand-region")

   "w" '(:ignore t :which-key "windows")
   "wd" 'delete-window
   "wm" '(delete-other-windows :which-key "maximise window")
   "wu" 'winner-undo

   "y" '(:ignore t :which-key "yasnippet")
   "yi" 'yas-insert-snippet
   "yr" 'yas-reload-all

   "/" 'helm-swoop-without-pre-input
   "1" 'other-window
   "=" 'gf/indent-buffer
   "+" 'gf/indent-cleanup-buffer
   ";" 'evilnc-comment-or-uncomment-lines
   "*" '(hydra-rotate-text/body :which-key "rotate text")
   "\\" 'align-regexp
   "TAB" 'previous-buffer)

  (general-define-key
   :states '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line)

  (general-define-key
   :states '(insert)
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line)

  (general-define-key
   "M-q" 'gf/close-buffer-other-window
   "M-x" 'helm-M-x))

(provide 'setup-keys)

;; (evil-declare-key 'normal org-mode-map (kbd "gn") 'gf/org-go-to-next-task)

;; (evil-declare-key 'insert org-mode-map (kbd "M-<return>") (lambda()
;;                                 (interactive)
;;                                 (evil-append-line 1)
;;                                 (org-meta-return)
;;                                 ))
;; (evil-declare-key 'normal org-mode-map (kbd "M-<return>") (lambda()
;;                                 (interactive)
;;                                 (evil-append-line 1)
;;                                 (org-meta-return)
;;                                 ))
;; (evil-declare-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
;; (evil-declare-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

;; (define-key global-map (kbd "C-c a") 'org-agenda)

;; (define-key global-map (kbd "C-c l") 'org-store-link)
;; ;;; to insert the link into an org mode buffer, use C-c C-l

;; ;; Vim style navigation
;; (define-key org-mode-map (kbd "C-c h") 'outline-up-heading)
;; (define-key org-mode-map (kbd "C-c j") 'outline-next-visible-heading)
;; (define-key org-mode-map (kbd "C-c k") 'outline-previous-visible-heading)
;; (define-key org-mode-map (kbd "C-c g") 'gf/org-end-of-section)
;; (define-key org-mode-map (kbd "C-c J") 'org-forward-heading-same-level)
;; (define-key org-mode-map (kbd "C-c K") 'org-backward-heading-same-level)

;; (evil-declare-key 'normal org-mode-map ",N" 'org-narrow-to-subtree)

;; (evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-display-inline-images)
;; (evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-remove-inline-images)

;; (evil-declare-key 'normal org-mode-map ",e" 'org-ctrl-c-ctrl-c)

;; (evil-declare-key 'normal org-mode-map ",a" 'helm-org-headlines)


;; (evil-declare-key 'normal php-mode-map ",z" 'gf/toggle-php-web-mode)
;; (evil-declare-key 'normal web-mode-map ",z" 'gf/toggle-php-web-mode)
