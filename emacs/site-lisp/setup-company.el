(use-package company :ensure t
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-limit 10)
  :config
  (global-company-mode)

  (general-define-key
   :keymaps '(company-active-map company-search-map company-filter-map)
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "TAB" #'nil)

  (diminish 'company-mode))

(provide 'setup-company)
