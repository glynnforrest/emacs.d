(use-package general :ensure t
  :config
  (setq
   gf/major-mode-leader-key "SPC m"
   gf/major-mode-non-normal-leader-key "M-m")

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "a" 'counsel-imenu

   "b" '(:ignore t :which-key "buffers")
   "bd" 'kill-this-buffer
   "bb" 'switch-to-buffer

   "e" '(:ignore t :which-key "emacs/eval")
   "eb" 'eval-buffer

   "f" '(:ignore t :which-key "files")
   "fD" 'delete-current-buffer-file
   "fe" 'gf/find-emacs-d-file
   "ff" 'find-file
   "fi" '(gf/open-init-file :which-key "open init.el")
   "fI" '(gf/open-personal-file :which-key "open setup-personal")
   "fo" 'gf/find-notes-file
   "fp" 'projectile-find-file
   "fr" 'counsel-recentf
   "fR" 'rename-current-buffer-file
   "fs" 'save-buffer

   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gt" '(gf/git-timemachine :which-key "git-timemachine")

   "m" '(:ignore t :which-key "major-mode")

   "o" '(:ignore t :which-key "org")
   "oc" 'org-capture
   "oN" 'gf/commit-notes
   "om" 'gf/find-current-month-notes-file
   "op" 'gf/toggle-switch-to-project-org-file

   "p" '(:ignore t :which-key "projects")
   "pk" 'projectile-kill-buffers
   "ps" 'projectile-switch-project

   "q" '(:ignore t :which-key "quitting")
   "qf" 'delete-frame
   "qq" 'save-buffers-kill-emacs
   "qQ" 'gf/save-buffers-kill-emacs-no-prompt

   "s" '(:ignore t :which-key "search")
   "sp" 'counsel-ag
   "so" 'org-search-view

   "t" '(:ignore t :which-key "toggle")
   "tr" '(rainbow-mode)
   "tg" '(global-git-gutter-mode :which-key "git-gutter")
   "tw" '(global-whitespace-mode :which-key "whitespace")
   "tW" '(ws-butler-mode :which-key "whitespace butler")

   "u" 'universal-argument

   "w" '(:ignore t :which-key "windows")
   "wm" '(delete-other-windows :which-key "maximise window")
   "wu" 'winner-undo

   "y" '(:ignore t :which-key "yasnippet")
   "yi" 'yas-insert-snippet
   "yr" 'yas-reload-all

   "/" 'swiper
   "1" 'other-window
   "=" 'gf/indent-buffer
   "+" 'gf/indent-cleanup-buffer
   ";" 'evilnc-comment-or-uncomment-lines
   "TAB" 'previous-buffer)

  (general-define-key
   "M-x" 'counsel-M-x))

(provide 'setup-keys)

;; (evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-shiftright)
;; (evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-shiftright)
;; (evil-declare-key 'normal org-mode-map (kbd "C-S-t") 'org-shiftleft)
;; (evil-declare-key 'insert org-mode-map (kbd "C-S-t") 'org-shiftleft)

;; (evil-declare-key 'normal org-mode-map (kbd "gn") 'gf/org-go-to-next-task)
;; (define-key org-mode-map (kbd "C-c t") 'org-todo)

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

;; (define-key org-mode-map (kbd "C-S-<up>") 'delete-other-windows)
;; (define-key org-mode-map (kbd "C-j") 'evil-window-down)
;; (define-key org-mode-map (kbd "C-k") 'evil-window-up)
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

;; (evil-declare-key 'normal org-mode-map "^" 'gf/evil-org-beginning-of-line)
;; (evil-declare-key 'normal org-mode-map "I"
;;   (lambda ()
;;     (interactive)
;;     (gf/evil-org-beginning-of-line)
;;     (evil-insert 1)
;;     ))

;; (evil-declare-key 'normal org-mode-map ",N" 'org-narrow-to-subtree)

;; (evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-display-inline-images)
;; (evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-remove-inline-images)

;; (evil-declare-key 'normal org-mode-map ",e" 'org-ctrl-c-ctrl-c)

;; (evil-declare-key 'normal org-mode-map ",a" 'helm-org-headlines)


;; (evil-declare-key 'normal php-mode-map ",z" 'gf/toggle-php-web-mode)
;; (evil-declare-key 'normal web-mode-map ",z" 'gf/toggle-php-web-mode)
