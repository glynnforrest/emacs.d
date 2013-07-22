;; This file adds support for evil mode in multiple cursors mode.

(require 'multiple-cursors)
(require 'evil)


(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-words-like-this)

;;; autopair messes with multiple cursors, so disable it
(add-hook 'multiple-cursors-mode-enabled-hook (lambda ()
                                                (autopair-mode -1)))
(add-hook 'multiple-cursors-mode-disabled-hook (lambda ()
                                                (autopair-mode t)))

;;; Thanks to tkf on
;;; https://github.com/magnars/multiple-cursors.el/issues/19
;;; insert state has been changed to emacs state
(defvar my-mc-evil-previous-state nil)

(defun my-mc-evil-switch-to-emacs-state ()
  (when (and (bound-and-true-p evil-mode)
             (not (eq evil-state 'emacs)))
    (setq my-mc-evil-previous-state evil-state)
    (evil-emacs-state)))

(defun my-mc-evil-back-to-previous-state ()
  (when my-mc-evil-previous-state
    (unwind-protect
        (case my-mc-evil-previous-state
          ((normal visual insert) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      my-mc-evil-previous-state)))
      (setq my-mc-evil-previous-state nil))))

(add-hook 'multiple-cursors-mode-enabled-hook
          'my-mc-evil-switch-to-emacs-state)
(add-hook 'multiple-cursors-mode-disabled-hook
          'my-mc-evil-back-to-previous-state)


(provide 'setup-multiple-cursors)
