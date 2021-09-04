(eval-when-compile (require 'use-package))

(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/leader-key
   :non-normal-prefix gf/non-normal-leader-key

   "a" 'imenu

   "b" '(:ignore t :which-key "buffers")
   "bb" 'switch-to-buffer
   "bd" 'kill-this-buffer
   "bD" 'kill-matching-buffers
   "bm" 'gf/switch-to-messages-buffer
   "bs" 'gf/switch-to-scratch-buffer

   "d" '(:ignore t :which-key "dired")
   "dc" `(,(gf/key 'dired "~/code") :which-key "~/code")
   "dd" 'dired
   "dj" 'dired-jump
   "dh" `(,(gf/key 'dired "~/Desktop") :which-key "~/Desktop")
   "dH" `(,(gf/key 'dired "~") :which-key "~")
   "dp" 'projectile-find-dir
   "dP" 'projectile-dired

   "e" '(:ignore t :which-key "emacs/eval/editor")
   "eb" 'eval-buffer
   "ec" 'editorconfig-display-current-properties
   "eC" 'editorconfig-apply
   "em" 'gf/show-previous-sexp-macro-expand
   "ep" 'package-list-packages
   "eP" 'package-autoremove

   "f" '(:ignore t :which-key "files")
   "fc" `(,(gf/key 'gf/helm-find-in-directory "~/code/") :which-key "~/code")
   "fD" 'vnd/delete-current-buffer-file
   "fe" 'gf/find-emacs-d-file
   "fE" 'editorconfig-find-current-editorconfig
   "ff" 'find-file
   "fF" 'find-function
   "fh" `(,(gf/key 'gf/helm-find-in-directory "~/Desktop/") :which-key "~/Desktop")
   "fH" `(,(gf/key 'gf/helm-find-in-directory "~/") :which-key "~")
   "fi" '(gf/open-init-file :which-key "open init.el")
   "fI" '(gf/open-personal-file :which-key "open setup-personal.el")
   "fo" 'gf/org-find-file
   "fp" 'projectile-find-file
   "fr" 'consult-recent-file
   "fR" 'vnd/rename-current-buffer-file
   "fs" 'save-buffer
   "fw" 'write-file
   "fx" 'gf/make-current-file-executable
   "fz" '(gf/open-zshrc :which-key "open .zshrc")
   "fZ" '(gf/open-zshrc-local :which-key "open .zshrc.local")
   "f." '(gf/open-dotenv-file :which-key "open dotenv for this project")

   "h" '(:ignore t :which-key "docs")
   "ht" 'gf/php-show-date-format-help

   "g" '(:ignore t :which-key "git")
   "gb" 'magit-blame
   "gs" 'magit-status
   "gt" '(gf/git-timemachine :which-key "git-timemachine")

   "G" 'git-gutter:revert-hunk

   "i" '(:ignore t :which-key "insert")
   "ib" '((lambda () (interactive) (insert (gf/buffer-file-name-body))) :which-key "gf/buffer-file-name-body")
   "ic" 'gf/php-insert-class
   "ir" 'gf/php-insert-symfony-route
   "is" 'gf/php-insert-service
   "it" 'gf/php-insert-symfony-twig-helper
   "iu" '((lambda () (interactive) (insert (gf/uuid))) :which-key "gf/uuid")
   "i4" '((lambda () (interactive) (insert (gf/hexstring 4))) :which-key "4-char hex")
   "i6" '((lambda () (interactive) (insert (gf/hexstring 6))) :which-key "6-char hex")
   "i8" '((lambda () (interactive) (insert (gf/hexstring 8))) :which-key "8-char hex")

   "j" 'vnd/arrayify

   "l" 'evil-lisp-state

   "m" '(:ignore t :which-key "major-mode")

   "n" '(:ignore t :which-key "narrowing")
   "nf" 'narrow-to-defun
   "nr" 'narrow-to-region
   "nw" 'widen

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

   "P" 'yank-from-kill-ring

   "q" '(:ignore t :which-key "quitting")
   "qf" 'delete-frame
   "qq" 'save-buffers-kill-emacs
   "qQ" 'gf/save-buffers-kill-emacs-no-prompt
   "qr" 'restart-emacs

   "r" 'gf/tmux-run-last
   "R" 'gf/tmux-run

   "s" '(:ignore t :which-key "search")
   "sp" 'consult-ripgrep
   "so" 'org-search-view

   "t" '(:ignore t :which-key "toggle")
   "tr" 'rainbow-mode
   "tg" '(global-git-gutter-mode :which-key "git-gutter")
   "tl" 'lsp-mode
   "tw" '(global-whitespace-mode :which-key "whitespace")
   "ts" '(flyspell-mode :which-key "flyspell")
   "tp" '(smartparens-mode :which-key "smartparens")
   "tW" '(ws-butler-mode :which-key "whitespace butler")

   "T" 'try-code

   "u" 'universal-argument
   "U" 'undo-tree-visualize

   "v" '(er/expand-region :which-key "expand-region")

   "w" '(:ignore t :which-key "what")
   "wf" 'gf/face-at-point

   "x" 'execute-extended-command

   "y" '(:ignore t :which-key "yasnippet")
   "yi" 'yas-insert-snippet
   "yr" 'yas-reload-all

   ";" 'consult-line
   "=" 'gf/indent-buffer
   "-" 'gf/untabify-buffer
   "+" 'gf/indent-cleanup-buffer
   "!" 'flycheck-next-error
   "*" '(hydra-rotate-text/body :which-key "rotate text")
   "$" 'gf/refresh-major-mode
   "\\" 'align-regexp)

  (general-define-key
   :states '(normal)
   "g+" 'evil-numbers/inc-at-pt
   "g=" 'evil-numbers/inc-at-pt
   "g-" 'evil-numbers/dec-at-pt)

  (general-define-key
   :states '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line
   "L" 'gf/evil-forward-arg
   "H" 'evil-backward-arg
   "K" 'evil-jump-out-args

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
   :keymaps 'evil-window-map
   "d" 'delete-window
   "m" '(delete-other-windows :which-key "maximise window")
   "u" 'winner-undo
   "U" 'winner-redo
   "." 'vnd/alternate-buffer
   "C-." 'vnd/alternate-buffer)

  (general-define-key
   "M-q" 'gf/close-buffer-other-window
   "M-d" 'scroll-other-window
   "M-u" 'scroll-other-window-down))

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
