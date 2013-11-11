(require 'elscreen)
(elscreen-start)

;; Share emacs
(require 'server)
(unless (server-running-p)
  (server-start))

;;Allows launching from chrome textareas
(require 'edit-server nil t)
(unless (process-status "edit-server")
  (setq edit-server-new-frame t)
  (edit-server-start))

(require 'setup-flyspell)

;;; Ido
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
(setq ido-ignore-buffers (append '("^\*Completions\*" "^\*Help\*" "^\*magit-process\*" "^\*Compile-Log\*" "^\*vc-diff\*") ido-ignore-buffers))
(setq ido-auto-merge-delay-time 99999)
(add-hook 'ido-setup-hook (lambda ()
                            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

;; Ido for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Better ido matching with flx
(require 'flx-ido)
(ido-mode 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Display ido vertically
(require 'ido-vertical-mode)

(ido-vertical-mode t)
(add-hook 'ido-setup-hook (lambda ()
                            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
                            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
                            ))

;;; General modes
(delete-selection-mode t)
(recentf-mode 1)
(show-paren-mode t)
(column-number-mode t)
(tooltip-mode -1)
;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)
(require 'ace-jump-mode)

;;; General settings
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(setq auto-save-default nil)
(setq hl-line-sticky-flag 1)
(setq ido-default-buffer-method 'selected-window)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq x-select-enable-clipboard t)
(setq undo-tree-visualizer-timestamps 1)

;; KILL SYSTEM BELL
(setq ring-bell-function #'ignore)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;; Enable normally disabled functions
(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Automatically create directories when creating a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


;; Load ssh credentials from keychain, even if keychain was called
;; after emacs startup
(require 'keychain-environment)
(define-key evil-normal-state-map ",k" (lambda ()
                                         (interactive)
                                         (keychain-refresh-environment)
                                         (message "Keychain environment refreshed.")))

;; refresh the current major mode
(define-key global-map (kbd "<f6>") (lambda ()
                                      (interactive)
                                      (call-interactively major-mode)))

(provide 'setup-general)
