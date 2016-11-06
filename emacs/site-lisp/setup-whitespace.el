;; May whitespace never bother me again
(use-package ws-butler :ensure t
  :config
  ;; Use spaces by default, override in individual modes with hooks.
  ;; The general rule of thumb is 4 spaces, with some mode-specific
  ;; exceptions.
  (setq-default
   c-basic-offset 4
   tab-width 4
   indent-tabs-mode nil)

  ;; Final newline is important
  ;; http://robots.thoughtbot.com/no-newline-at-end-of-file
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  (ws-butler-global-mode t))

(defun gf/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun gf/indent-cleanup-buffer ()
  "Indent and cleanup the whitespace of the entire buffer."
  (interactive)
  (gf/indent-buffer)
  (ws-butler-clean-region (point-min) (point-max)))

;; Change to unix line endings when loading a DOS file
;; http://www.emacswiki.org/emacs/DosToUnix
(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(provide 'setup-whitespace)
