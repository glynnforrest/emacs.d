;; Use spaces by default, override in individual modes with hooks.
;; The general rule of thumb for this setup is 4 spaces, with some
;; mode-specific exceptions.
(setq-default
 c-basic-offset 4
 tab-width 4
 indent-tabs-mode nil
 )

;; Pretty much all whitespace related stuff is covered with
;; ethan-wspace.
;; May whitespace never bother me again
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; Final newline is important
;; http://robots.thoughtbot.com/no-newline-at-end-of-file
;; Ethan mode subsumes all of these settings, so we have to set them as nil.
;; Final newline is required with ethan-mode.
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; Show whitespace if paranoid
(define-key global-map (kbd "C-c w") 'whitespace-mode)

(defun gf/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun gf/indent-cleanup-buffer ()
  "Indent and cleanup the whitespace of the entire buffer."
  (interactive)
  (gf/indent-buffer)
  (ethan-wspace-clean-all))

(define-key evil-normal-state-map ",=" 'gf/indent-buffer)
(define-key evil-normal-state-map ",+" 'gf/indent-cleanup-buffer)

;; Unfortunately some file types use tabs. Tell ethan-wspace to go
;; easy on them.
(defun tabs-are-less-evil ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(add-hook 'makefile-mode-hook 'tabs-are-less-evil)
(add-hook 'git-commit-mode-hook 'tabs-are-less-evil)

;; Change to unix line endings when loading a DOS file
;; http://www.emacswiki.org/emacs/DosToUnix

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(define-key evil-normal-state-map ",,=" 'dos2unix)

(provide 'setup-whitespace)
