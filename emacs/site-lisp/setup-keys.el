(defvar gf/leader-key "SPC"
  "Leader key prefix to use for key bindings.")

(defvar gf/non-normal-leader-key "M-SPC"
  "Leader key prefix to use for key bindings in non-normal evil modes.")

(defvar gf/major-mode-leader-key "SPC m"
  "Leader key prefix to use for major mode key bindings.")

(defvar gf/major-mode-non-normal-leader-key "M-SPC m"
  "Leader key prefix to use for major mode key bindings in non-normal evil modes.")

(defmacro gf/key (cmd &rest args)
  "Helper macro to add an argument to a key binding.

(general-define-key
  \"a\" (gf/key 'command-name \"arg1\" \"arg2\"))

or if using plists

(general-define-key
  \"a\" `(,(gf/key 'command-name \"arg1\" \"arg2\") :which-key \"run command-name\"))
"
  `(lambda () (interactive) ,(append (cdr cmd) args)))

(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/leader-key
   :non-normal-prefix gf/non-normal-leader-key

   "a" 'helm-imenu

   "b" '(:ignore t :which-key "buffers")
   "bb" 'helm-mini
   "bd" 'kill-this-buffer
   "bD" 'kill-matching-buffers
   "bm" 'gf/switch-to-messages-buffer
   "bs" 'gf/switch-to-scratch-buffer

   "d" '(:ignore t :which-key "dired")
   "dd" 'dired
   "df" 'projectile-find-dir
   "dj" 'dired-jump
   "dp" 'projectile-dired

   "e" '(:ignore t :which-key "emacs/eval")
   "eb" 'eval-buffer
   "em" 'gf/show-previous-sexp-macro-expand
   "ep" 'package-list-packages
   "eP" 'package-autoremove

   "f" '(:ignore t :which-key "files")
   "fD" 'delete-current-buffer-file
   "fe" 'gf/find-emacs-d-file
   "ff" 'helm-find-files
   "fh" `(,(gf/key 'gf/helm-find-in-directory "~/") :which-key "find in home directory")
   "fH" `(,(gf/key 'gf/helm-find-in-directory "~/Desktop/") :which-key "find on the desktop")
   "fi" '(gf/open-init-file :which-key "open init.el")
   "fI" '(gf/open-personal-file :which-key "open setup-personal.el")
   "fo" 'gf/org-find-file
   "fp" 'projectile-find-file
   "fr" 'helm-recentf
   "fR" 'rename-current-buffer-file
   "fs" 'save-buffer
   "fw" 'write-file
   "fx" 'gf/make-current-file-executable
   "fz" '(gf/open-zshrc :which-key "open .zshrc")
   "fZ" '(gf/open-zshrc-local :which-key "open .zshrc.local")

   "h" '(:ignore t :which-key "docs")
   "hd" 'helm-dash
   "hD" 'helm-dash-at-point
   "ht" 'gf/php-show-date-format-help

   "g" '(:ignore t :which-key "git/gtags")
   "gb" 'magit-blame
   "gc" 'gf/gtags-generate-for-project
   "gd" 'gf/gtags-delete-for-project
   "gg" 'helm-gtags-select
   "gs" 'magit-status
   "gt" '(gf/git-timemachine :which-key "git-timemachine")
   "gu" 'helm-gtags-update-tags

   "G" 'git-gutter:revert-hunk

   "i" '(:ignore t :which-key "insert")
   "ic" 'gf/php-insert-class
   "ir" 'gf/php-insert-symfony-route
   "is" 'gf/php-insert-service
   "it" 'gf/php-insert-symfony-twig-helper

   "l" 'evil-lisp-state

   "m" '(:ignore t :which-key "major-mode")

   "o" '(:ignore t :which-key "org")
   "oi" '(gf/find-org-info-file :which-key "open info file")
   "ol" 'org-capture-goto-last-stored
   "oL" 'org-refile-goto-last-stored
   "oN" 'gf/commit-notes
   "om" 'gf/find-current-month-notes-file
   "oo" 'org-capture ; as in, "oo, I have an idea"
   "op" 'gf/projects-toggle-org-file

   "p" '(:ignore t :which-key "projects")
   "pk" 'projectile-kill-buffers
   "ps" 'projectile-switch-project
   "pS" 'projectile-save-project-buffers
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
   "sp" 'helm-do-ag-project-root
   "so" 'org-search-view

   "t" '(:ignore t :which-key "toggle")
   "tr" '(rainbow-mode)
   "tg" '(global-git-gutter-mode :which-key "git-gutter")
   "tl" '(lsp-mode)
   "tw" '(global-whitespace-mode :which-key "whitespace")
   "ts" '(flyspell-mode :which-key "flyspell")
   "tp" '(smartparens-mode :which-key "smartparens")
   "tW" '(ws-butler-mode :which-key "whitespace butler")

   "T" 'try-code

   "u" 'universal-argument
   "U" 'undo-tree-visualize

   "v" '(er/expand-region :which-key "expand-region")

   "w" '(:ignore t :which-key "windows/what")
   "wd" 'delete-window
   "wf" 'gf/face-at-point
   "wm" '(delete-other-windows :which-key "maximise window")
   "wr" 'winner-redo
   "wu" 'winner-undo

   "y" '(:ignore t :which-key "yasnippet")
   "yi" 'yas-insert-snippet
   "yr" 'yas-reload-all

   "." 'helm-gtags-dwim
   "," 'helm-gtags-pop-stack

   "/" 'helm-swoop
   ";" 'helm-swoop-without-pre-input
   "1" 'other-window
   "=" 'gf/indent-buffer
   "+" 'gf/indent-cleanup-buffer
   "!" 'flycheck-next-error
   "*" '(hydra-rotate-text/body :which-key "rotate text")
   "$" 'gf/refresh-major-mode
   "\\" 'align-regexp
   "TAB" 'alternate-buffer)

  (general-define-key
   :states '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line

   "M-j" 'move-text-down
   "M-k" 'move-text-up)

  (general-define-key
   :states '(insert)
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line)

  (general-define-key
   :states '(visual)
   "<" 'gf/visual-shift-left
   ">" 'gf/visual-shift-right
   "K" 'sort-lines

   ;; fix move-text for visual mode
   "M-j" (concat ":m '>+1" (kbd "RET") "gv=gv")
   "M-k" (concat ":m '<-2" (kbd "RET") "gv=gv"))

  (general-define-key
   "M-q" 'gf/close-buffer-other-window
   "M-x" 'helm-M-x))

(provide 'setup-keys)

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
