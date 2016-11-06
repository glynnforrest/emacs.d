(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"

   "b" '(:ignore t :which-key "buffers")
   "bd" 'kill-this-buffer
   "bb" 'switch-to-buffer

   "e" '(:ignore t :which-key "emacs/eval")
   "eb" 'eval-buffer

   "f" '(:ignore t :which-key "files")
   "fd" 'delete-current-buffer-file
   "ff" 'find-file
   "fp" 'projectile-find-file
   "fr" 'rename-current-buffer-file
   "fs" 'save-buffer

   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status

   "p" '(:ignore t :which-key "projects")
   "pk" 'projectile-kill-buffers

   "q" '(:ignore t :which-key "quitting")
   "qq" 'save-buffers-kill-emacs

   "s" '(:ignore t :which-key "search")
   "sp" 'counsel-ag
   "so" 'swiper

   "t" '(:ignore t :which-key "toggle")
   "tw" '(global-whitespace-mode :which-key "whitespace")
   "tW" '(ws-butler-mode :which-key "whitespace butler")

   "w" '(:ignore t :which-key "windows")
   "wu" 'winner-undo

   "1" 'other-window
   "TAB" 'previous-buffer)

  (general-define-key
   "M-x" 'counsel-M-x)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "TAB" 'org-cycle)
  )

(provide 'setup-keys)

;; (mapcar (lambda (state)
;;       (evil-declare-key state org-mode-map
;;         (kbd "M-l") 'org-metaright
;;         (kbd "M-h") 'org-metaleft
;;         (kbd "M-k") 'org-metaup
;;         (kbd "M-j") 'org-metadown
;;         (kbd "M-L") 'org-shiftmetaright
;;         (kbd "M-H") 'org-shiftmetaleft
;;         (kbd "M-K") 'org-shiftmetaup
;;         (kbd "M-J") 'org-shiftmetadown))
;;     '(normal insert))

;; ;; quick hotkey for searching notes
;; (define-key global-map (kbd "C-c n") 'org-search-view)

;; (define-key global-map (kbd "C-x C-n") 'gf/commit-notes)

;; (define-key global-map (kbd "M-n") 'org-capture)
;; (define-key global-map (kbd "M-N") 'gf/find-current-month-notes-file)
;; (define-key global-map (kbd "C-c C-n") (lambda ()
;;                                        (interactive)
;;                                        (projectile-find-file-in-directory org-directory)))

;; (evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-shiftright)
;; (evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-shiftright)
;; (evil-declare-key 'normal org-mode-map (kbd "C-S-t") 'org-shiftleft)
;; (evil-declare-key 'insert org-mode-map (kbd "C-S-t") 'org-shiftleft)

;; (evil-declare-key 'normal org-mode-map (kbd "gn") 'gf/org-go-to-next-task)
;; (define-key org-mode-map (kbd "C-c t") 'org-todo)

;; ;; refile over files
;; (evil-declare-key 'normal org-mode-map (kbd "C-c r") 'gf/org-refile-files-first)
;; (evil-declare-key 'visual org-mode-map (kbd "C-c r") 'gf/org-refile-files-first)

;; ;; refile withing the same file
;; (evil-declare-key 'normal org-mode-map (kbd "C-c R") 'org-refile)
;; (evil-declare-key 'visual org-mode-map (kbd "C-c R") 'org-refile)

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

;; (evil-declare-key 'normal css-mode-map "gc" 'cssEvilChangeToPX)
;; (evil-declare-key 'normal css-mode-map ",e" 'skewer-css-eval-current-rule)

;; (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

;; (evil-declare-key 'normal php-mode-map ",z" 'gf/toggle-php-web-mode)
;; (evil-declare-key 'normal web-mode-map ",z" 'gf/toggle-php-web-mode)
;; (define-key php-mode-map (kbd "C-c i") 'gf/php-insert-use-class)
;; (define-key php-mode-map (kbd "C-c I") 'gf/php-insert-class)
;; (define-key php-mode-map (kbd "C-c s") 'gf/php-insert-service)

;; (define-key php-mode-map (kbd "M-q") 'gf/quit-other-window)
