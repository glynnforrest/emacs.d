(use-package company :ensure t
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-limit 10)

  (general-define-key
   :keymaps '(company-active-map company-search-map company-filter-map)
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "TAB" nil))


(when (not (version< emacs-version "25.1"))
  (use-package company-lsp :ensure t
    :init
    (push 'company-lsp company-backends)))

(provide 'setup-company)
