;; Use spaces by default, override in individual modes with hooks.
;; The general rule of thumb for this setup is 4 spaces, with some
;; mode-specific exceptions.
(setq-default
 c-basic-offset 4
 tab-width 4
 indent-tabs-mode nil
 )

(define-key global-map (kbd "C-c w") 'global-whitespace-mode)

(defun gf/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; Indent buffer, cleanup whitespace problems and enforce tabs or
;; spaces
(defun gf/indent-cleanup-buffer ()
  "Indent and cleanup the whitespace of the entire buffer."
  (interactive)
  (gf/indent-buffer)
  (whitespace-cleanup))

(define-key evil-normal-state-map ",=" 'gf/indent-buffer)
(define-key evil-normal-state-map ",+" 'gf/indent-cleanup-buffer)

;; Remove any trailing whitespace on buffer write
(define-minor-mode remove-trailing-whitespace-mode
  "Toggle remove trailing whitespace on save.
When enabled trailing whitespace is removed before saving."
  :init-value nil
  :global t
  :lighter " W"

  (if remove-trailing-whitespace-mode
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

(remove-trailing-whitespace-mode t)

(require 'highlight-chars)
(add-hook 'prog-mode-hook 'hc-highlight-tabs)

;; Change to unix line endings when loading a DOS file
;; http://www.emacswiki.org/emacs/DosToUnix

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(define-key evil-normal-state-map ",,=" 'dos2unix)

(provide 'setup-whitespace)
