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


  (general-define-key
   :keymaps 'helm-map
   "C-j" 'helm-next-line
   "C-k" 'helm-previous-line
   "C-h" 'helm-next-source
   "C-l" (kbd "RET"))

  (use-package helm-files
    :config
    (general-define-key
     :keymaps '(helm-find-files-map helm-read-file-map)
     "C-l" 'helm-execute-persistent-action
     "C-h" 'helm-find-files-up-one-level)))

(use-package helm-css-scss :ensure t
  :config
  (setq helm-css-scss-split-with-multiple-windows nil))

(use-package helm-swoop :ensure t)

;; helm-ls-git can be used for grep / occur on many files at a time
;; (require 'helm-ls-git)

(provide 'setup-helm)
