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

;;; General modes
(delete-selection-mode t)
(recentf-mode 1)
(setq recentf-max-saved-items 2000)
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
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq x-select-enable-clipboard t)
(setq undo-tree-visualizer-timestamps 1)

;; http://robots.thoughtbot.com/no-newline-at-end-of-file
(setq require-final-newline t)

;; KILL SYSTEM BELL
(setq ring-bell-function #'ignore)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;; Enable normally disabled functions
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

;; emacs doesn't actually save undo history with revert-buffer
;; see http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00151.html
;; fix that.
;; http://stackoverflow.com/questions/4924389/is-there-a-way-to-retain-the-undo-list-in-emacs-after-reverting-a-buffer-from-fi
(defun revert-buffer-keep-history (&optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
  (interactive)

  ;; tell Emacs the modtime is fine, so we can edit the buffer
  (clear-visited-file-modtime)

  ;; insert the current contents of the file on disk
  (widen)
  (delete-region (point-min) (point-max))
  (insert-file-contents (buffer-file-name))

  (save-buffer)
  (set-visited-file-modtime))

(setq revert-buffer-function 'revert-buffer-keep-history)

(provide 'setup-general)
