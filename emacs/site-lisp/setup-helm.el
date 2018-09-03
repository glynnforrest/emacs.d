;; https://tuhdo.github.io/helm-intro.html has great helm tips.
(use-package helm :ensure t
  :diminish ""
  :config

  (setq helm-move-to-line-cycle-in-source nil
        helm-split-window-default-side 'other
        helm-split-window-in-side-p t
        helm-display-header-line nil
        helm-candidate-number-limit 200
        helm-M-x-requires-pattern 0
        helm-net-prefer-curl-p t
        helm-buffer-max-length nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)

  (set-face-attribute 'helm-source-header nil :height 0.1)

  (helm-mode)
  (helm-autoresize-mode t)

  (defun gf/helm-find-in-directory (start)
    (interactive)
    (let ((default-directory (expand-file-name start)))
      (call-interactively 'helm-find-files)))

  (general-define-key
   :keymaps 'helm-map
   "C-j" 'helm-next-line
   "C-k" 'helm-previous-line
   "C-h" 'helm-next-source
   "C-l" (kbd "RET")))

(use-package helm-files
  :after helm
  :config
  (general-define-key
   :keymaps '(helm-find-files-map helm-read-file-map)
   "C-l" 'helm-execute-persistent-action
   "C-h" 'helm-find-files-up-one-level))

(use-package helm-ag :ensure t
  :after helm
  :config
  (setq helm-ag-base-command "rg --smart-case --no-heading --vimgrep")

  (defun gf/helm-ag-goto ()
    "Go to the occurrence on the current line and recenter."
    (interactive)
    (helm-ag-mode-jump-other-window)
    (recenter))

  (defun gf/helm-ag-show ()
    "Show a compilation in the other window, but stay in the compilation buffer."
    (interactive)
    (gf/helm-ag-goto)
    (other-window -1))

  (general-define-key
   :keymaps 'helm-do-ag-map
   "M-RET" 'helm-ag--run-save-buffer
   "M-e" 'helm-ag-edit)

  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'helm-ag-mode-map
   "q" 'kill-this-buffer)

  (general-define-key
   :states '(normal)
   :keymaps 'helm-ag-mode-map
   "RET" 'gf/helm-ag-show
   "M-RET" 'gf/helm-ag-goto))

(use-package helm-css-scss :ensure t
  :config
  (setq helm-css-scss-split-with-multiple-windows nil))

(use-package helm-swoop :ensure t
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-split-direction 'split-window-horizontally
        helm-swoop-use-line-number-face t))

(provide 'setup-helm)
