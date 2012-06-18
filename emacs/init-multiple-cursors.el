;; This file adds support for evil mode in multiple cursors mode.

(require 'multiple-cursors)
(require 'mark-more-like-this)
(require 'evil)

(defun evil-mc-edit-beginnings-of-lines ()
  (interactive)
  (mc/edit-beginnings-of-lines)
  (evil-emacs-state)
  (electric-pair-mode -1)
  )

(defun evil-mc-edit-ends-of-lines ()
  (interactive)
  (mc/edit-ends-of-lines)
  (evil-emacs-state)
  (electric-pair-mode -1)
  )

(defun evil-mc-switch-to-cursors ()
  (interactive)
  (evil-emacs-state)
  (electric-pair-mode -1)
  (mc/switch-from-mark-multiple-to-cursors)
  )

(defadvice mc/keyboard-quit (after mc-evil-cleanup activate)
  (interactive)
  (evil-normal-state)
  (electric-pair-mode t)
  )

; (set-cursor-color "#92c48f")
(provide 'init-multiple-cursors)
