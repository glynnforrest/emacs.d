(use-package server :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package hydra :ensure t )

;; enable/disable built-in modes
(column-number-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(recentf-mode t)
(savehist-mode t)
(show-paren-mode t)
(tooltip-mode nil)
(winner-mode t)
(xterm-mouse-mode t)

(use-package autorevert
  :diminish ""
  :config
  (global-auto-revert-mode t))

;; sensible defaults 
(fset 'yes-or-no-p 'y-or-n-p)
(setq
 auto-revert-verbose nil
 auto-save-default nil
 create-lockfiles nil
 echo-keystrokes 0.1
 global-auto-revert-non-file-buffers t
 history-length 1000
 hl-line-sticky-flag 1
 inhibit-startup-message t
 make-backup-files nil
 mouse-wheel-progressive-speed nil
 recentf-max-saved-items 2000
 ring-bell-function #'ignore
 scroll-conservatively 10000
 scroll-step 1
 shift-select-mode nil
 undo-tree-visualizer-timestamps 1
 x-select-enable-clipboard t)

;; enable normally disabled functions
(put 'narrow-to-region 'disabled nil)

(use-package restart-emacs :ensure t)

(provide 'setup-core)
