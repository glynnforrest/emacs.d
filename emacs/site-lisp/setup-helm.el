;; https://tuhdo.github.io/helm-intro.html has great helm tips.
(use-package helm :ensure t
  :config

  (setq helm-move-to-line-cycle-in-source nil
        helm-split-window-default-side 'other
        helm-split-window-in-side-p t
        helm-candidate-number-limit 200
        helm-M-x-requires-pattern 0
        helm-google-suggest-use-curl-p t)

  (helm-mode)


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

;; helm-ls-git can be used for grep / occur on many files at a time
;; (require 'helm-ls-git)

(provide 'setup-helm)
