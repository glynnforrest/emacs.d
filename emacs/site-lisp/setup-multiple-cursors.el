(use-package multiple-cursors :ensure t
  :config
  ;; Thanks to tkf on
  ;; https://github.com/magnars/multiple-cursors.el/issues/19
  (defvar gf/mc-previous-state nil)

  (defun gf/mc-switch-to-emacs-state ()
    (when (and (bound-and-true-p evil-mode)
               (not (eq evil-state 'emacs)))
      (setq gf/mc-previous-state evil-state)
      (evil-emacs-state)))

  (defun gf/mc-back-to-previous-state ()
    (when gf/mc-previous-state
      (unwind-protect
          (case gf/mc-previous-state
            ((normal visual insert) (evil-force-normal-state))
            (t (message "Don't know how to handle previous state: %S"
                        gf/mc-previous-state)))
        (setq gf/mc-previous-state nil))))

  (add-hook 'multiple-cursors-mode-enabled-hook 'gf/mc-switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'gf/mc-back-to-previous-state))

(provide 'setup-multiple-cursors)
