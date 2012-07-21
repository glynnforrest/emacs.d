;; set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages '(
                            ace-jump-mode
                            auto-complete
                            color-theme-sanityinc-tomorrow
                            dired+
                            evil
                            fuzzy
                            js2-mode
                            js-comint
                            magit
                            multi-web-mode
                            org
                            php-mode
                            projectile
                            rainbow-delimiters
                            smex
                            surround
                            test-case-mode
                            yasnippet
                            zencoding-mode
                            )
  "A list of required packages for this setup.")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Set path to .emacs.d
(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))

;; Set path to manually installed plugins
(setq plugins-dir (expand-file-name "plugins" emacs-dir))


;; Set up load path
(let ((default-directory plugins-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path emacs-dir)
;; Load personal configurations, like usernames and passwords
(require 'personal)

;;evil
(require 'evil)
(evil-mode 1)
(setq evil-default-cursor t)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
;;Switch gj and j, gk and k
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)

(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-bright)

(setq hl-line-sticky-flag 1)
(global-hl-line-mode t)

(require 'surround)
(global-surround-mode t)
(global-auto-revert-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'projectile)
(projectile-global-mode 1)

(require 'test-case-mode)
(define-key evil-normal-state-map ",t" 'test-case-run)
(define-key evil-normal-state-map ",T" 'test-case-run-all)

;;yasnippet
(require 'yasnippet)
;; Don't use bundled snippets
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;;don't expand part of words
(setq yas/key-syntaxes '("w_" "w_." "^ "))

(require 'auto-complete-config)
;;yasnippet / auto-complete fix
(defun ac-yasnippet-candidates ()
  (with-no-warnings
    (if (fboundp 'yas/get-snippet-tables)
        ;; >0.6.0
        (apply 'append (mapcar 'ac-yasnippet-candidate-1
                               (condition-case nil
                                   (yas/get-snippet-tables major-mode)
                                 (wrong-number-of-arguments
                                  (yas/get-snippet-tables)))))
      (let ((table
             (if (fboundp 'yas/snippet-table)
                 ;; <0.6.0
                 (yas/snippet-table major-mode)
               ;; 0.6.0
               (yas/current-snippet-table))))
        (if table
            (ac-yasnippet-candidate-1 table))))))

(ac-define-source yasnippet-glynn
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (symbol . "snip")))

(defun ac-config-glynn ()
  (setq-default ac-sources '(ac-source-yasnippet-glynn ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook (lambda()
									(setq ac-sources (append '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols) ac-sources))))
  (add-hook 'css-mode-hook (lambda ()
							 (setq ac-sources (append '(ac-source-css-property) ac-sources))))
(global-auto-complete-mode t))

(require 'fuzzy)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-auto-start t)
(setq ac-use-fuzzy t)
(global-set-key (kbd "C-SPC") 'auto-complete)
(ac-config-glynn)

(require 'ace-jump-mode)
(define-key evil-normal-state-map ",m" 'ace-jump-mode)

(require 'php-mode)
(electric-pair-mode t)

(recentf-mode 1)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Open recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Prevent Emacs from auto-changing the working directory
(defun find-file-keep-directory ()
  (interactive)
  (setq saved-default-directory default-directory)
  (ido-find-file)
  (setq default-directory saved-default-directory))

;;Automatically create directories when creating a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(global-linum-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)
;;Use tabs
(setq-default c-basic-offset 4
			tab-width 4
			indent-tabs-mode t)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

;; Change buffers with left and right, Ctrl if not in evil-mode
(define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key global-map (kbd "C-<left>") 'previous-buffer)
(define-key global-map (kbd "C-<up>") 'delete-window)
(define-key global-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key global-map (kbd "C-S-<down>") (lambda ()
                                            (interactive)
                                            (kill-this-buffer)
                                            (delete-window)))
(define-key evil-normal-state-map (kbd "C-<down>") 'kill-this-buffer)
(define-key evil-insert-state-map (kbd "C-<right>") 'forward-word)
(define-key evil-insert-state-map (kbd "C-<left>") 'backward-word)

(require 'help-mode)
(define-key help-mode-map (kbd "C-<down>") 'kill-this-buffer)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map ",F" 'find-file)
(define-key evil-normal-state-map ",e" (lambda()
                                         (interactive)
                                         (call-interactively 'find-file)
                                         ;;(ido-magic-forward-char)
                                         ))
(define-key evil-normal-state-map ",r" 'recentf-ido-find-file)
(define-key evil-normal-state-map ",d" 'ido-dired)

(define-key evil-normal-state-map ",cd" (lambda ()
                                          (interactive)
                                          (switch-to-buffer "*scratch*")
                                          (call-interactively 'cd)))

(defun split-window-and-move-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-and-move-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(define-key evil-normal-state-map ",s" 'split-window-and-move-right)
(define-key evil-normal-state-map ",S" 'split-window-and-move-below)
(define-key evil-normal-state-map ",u" 'undo-tree-visualize)

;;Magit
(require 'magit)
(define-key evil-normal-state-map ",g" 'magit-status)
(evil-declare-key 'normal magit-log-edit-mode-map ",w" 'magit-log-edit-commit)
(define-key magit-mode-map "q" (lambda ()
                                 (interactive)
                                 (if (get-buffer "*magit-process*")
                                     (kill-buffer "*magit-process*"))
                                 (if (get-buffer "*magit-edit-log*")
                                     (kill-buffer "*magit-edit-log*"))
                                 (kill-this-buffer)
                                 ))

(setq undo-tree-visualizer-timestamps 1)

(define-key global-map (kbd "M-b") 'ido-switch-buffer)
(define-key evil-normal-state-map " " 'evil-ex)
(define-key evil-visual-state-map " " 'evil-ex)
(define-key evil-visual-state-map "n" 'narrow-to-region)
(define-key evil-normal-state-map ",n" 'widen)

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(define-key evil-normal-state-map ",i" 'open-init-file)

(defun my-save-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))
(define-key evil-normal-state-map ",I" 'my-save-and-eval-buffer)

(defun my-eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))


(define-key global-map (kbd "C-l") 'evil-window-right)
(define-key global-map (kbd "C-h") 'evil-window-left) ;; get help-map with f1
(define-key global-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-decrease-width)

(add-hook 'lisp-interaction-mode-hook (lambda()
                                        (local-unset-key (kbd "C-j"))
                                        (local-set-key (kbd "M-J") 'my-eval-print-last-sexp)))

(defun move-line-up-and-indent ()
  (interactive)
  (transpose-lines 1)
  (evil-previous-line 2)
  (indent-for-tab-command)
  )

(defun move-line-down-and-indent ()
  (interactive)
  (evil-next-line 1)
  (transpose-lines 1)
  (evil-previous-line 1)
  (indent-for-tab-command)
  )

(define-key evil-normal-state-map (kbd "M-j") 'move-line-down-and-indent)
(define-key evil-normal-state-map (kbd "M-k") 'move-line-up-and-indent)
;; nnoremap <A-l> >>
;; nnoremap <A-h> <<

(define-key evil-normal-state-map (kbd "<S-return>") 'my-evil-new-line)
(defun my-evil-new-line ()
  (interactive)
  (evil-open-below 1)
  (evil-normal-state 1))

(define-key evil-normal-state-map (kbd "<C-S-return>") 'my-evil-new-line-above)
(defun my-evil-new-line-above ()
  (interactive)
  (evil-open-above 1)
  (evil-normal-state 1))

;; universal escape key as well as C-g
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; Cursor configuration.
(blink-cursor-mode -1)
(setq evil-insert-state-cursor '("#ffffff" bar))
(setq evil-normal-state-cursor '("#ffffff" box))
(setq evil-emacs-state-cursor '("#d72626" bar))

(require 'init-multiple-cursors)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(define-key evil-visual-state-map "mb" 'evil-mc-edit-beginnings-of-lines)
(define-key evil-visual-state-map "me" 'evil-mc-edit-ends-of-lines)
(define-key evil-visual-state-map "mm" 'evil-mc-switch-to-cursors)


(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))
(define-key evil-normal-state-map ",E" 'eval-and-replace-sexp)

;; Multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "php4" "php5"))
(define-key evil-normal-state-map ",q" 'multi-web-global-mode)

(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
                            (kbd "M-l") 'org-metaright
                            (kbd "M-h") 'org-metaleft
                            (kbd "M-k") 'org-metaup
                            (kbd "M-j") 'org-metadown
                            (kbd "M-L") 'org-shiftmetaright
                            (kbd "M-H") 'org-shiftmetaleft
                            (kbd "M-K") 'org-shiftmetaup
                            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

;;Notes are grouped by months for automatic archival.
;;At the start of every month move over notes that are still relevant.
(setq org-default-notes-file (downcase (format-time-string "~/Notes/%Y-%B.org")))
(define-key global-map (kbd "M-m") 'org-capture)
(define-key global-map (kbd "M-M") (lambda()
                                     (interactive)
                                     (find-file org-default-notes-file)
                                     ))
(evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'normal org-mode-map (kbd "C-m") 'org-refile)
(evil-declare-key 'visual org-mode-map "m" 'org-refile)
(evil-declare-key 'insert org-mode-map (kbd "M-<return>") (lambda()
                                                            (interactive)
                                                            (evil-append-line 1)
                                                            (org-meta-return)
                                                            ))
(evil-declare-key 'normal org-mode-map (kbd "M-<return>") (lambda()
                                                            (interactive)
                                                            (evil-append-line 1)
                                                            (org-meta-return)
                                                            ))
(evil-declare-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
(define-key org-mode-map (kbd "C-S-<up>") 'delete-other-windows)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?")
        ("l" "Linked Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?\n%a")
        ("n" "Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?")
        ("h" "Linked Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?\n%a")))

;; Dired mode
(require 'dired)
(require 'dired+)
(add-hook 'dired-mode-hook (lambda ()
                             (interactive)
                             (rename-buffer "*Dired*")
                             ))

;;Up directory fix
(defadvice dired-up-directory (around dired-up-fix activate)
  (interactive)
  (rename-buffer "*Dired-old*")
  ad-do-it
  (previous-buffer)
  (kill-this-buffer)
  )

(evil-declare-key 'normal dired-mode-map ",e" (lambda ()
                                                (interactive)
                                                (dired-toggle-read-only)
                                                (evil-normal-state)
                                                (evil-forward-char)
                                                ))
(evil-declare-key 'normal dired-mode-map "\\" 'dired-up-directory)
(evil-declare-key 'normal dired-mode-map "q" 'evil-record-macro)
(evil-declare-key 'normal wdired-mode-map ",e" 'wdired-finish-edit)
(evil-declare-key 'normal wdired-mode-map ",a" 'wdired-abort-changes)
(define-key dired-mode-map (kbd "M-b") 'ido-switch-buffer)
(toggle-diredp-find-file-reuse-dir 1)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;;ERC
(require 'erc)
;;my-erc-nick should be in personal.el
(setq erc-nick my-erc-nick)
(add-hook 'erc-mode-hook (lambda () 
                           (interactive)
                           (linum-mode -1)))

;;eshell
(require 'eshell)
(define-key evil-normal-state-map ",x" (lambda ()
                                         (interactive)
                                         (eshell)
                                         ))
(evil-declare-key 'normal eshell-mode-map "i" (lambda ()
                                                (interactive)
                                                (evil-goto-line)
                                                (evil-append-line 1)
                                                ))
(evil-declare-key 'normal eshell-mode-map (kbd "C-j") 'evil-window-down)
(evil-declare-key 'insert eshell-mode-map (kbd "C-j") 'evil-window-down)
(evil-declare-key 'normal eshell-mode-map (kbd "C-<up>") 'delete-window)

;;node REPL
(require 'js-comint)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 node")

(add-to-list 'default-frame-alist '(font . "Consolas-10"))

;;Write room
(defvar writeroom-enabled nil)
(require 'hide-mode-line)

(defun toggle-writeroom ()
  (interactive)
  (if (not writeroom-enabled)
      (setq writeroom-enabled t)
    (setq writeroom-enabled nil))
  (hide-mode-line)
  (global-linum-mode -1)
  (if writeroom-enabled
      (progn
        (fringe-mode 'both)
        (menu-bar-mode -1)
        (set-fringe-mode 200))
    (progn 
      (fringe-mode 'default)
      (menu-bar-mode)
      (global-linum-mode 1)
	  (set-fringe-mode 8))))

(define-key global-map (kbd "<f9>") 'toggle-writeroom)
