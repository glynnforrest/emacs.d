(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package hydra )
(use-package diminish )

;; enable/disable built-in modes
(column-number-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(recentf-mode t)
(savehist-mode t)
(show-paren-mode t)
(tooltip-mode -1)
(winner-mode t)
(xterm-mouse-mode -1)

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
 hl-line-sticky-flag t
 inhibit-startup-message t
 make-backup-files nil
 mouse-wheel-progressive-speed nil
 recentf-max-saved-items 2000
 ring-bell-function #'ignore
 scroll-conservatively 10000
 scroll-step 1
 select-enable-clipboard t
 shift-select-mode nil
 undo-tree-visualizer-timestamps 1
 vc-follow-symlinks t)

;; enable normally disabled functions
(put 'narrow-to-region 'disabled nil)

(use-package restart-emacs)

(use-package s)

(provide 'setup-core)
