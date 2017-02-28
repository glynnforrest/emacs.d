(use-package company :ensure t
  :config
  (global-company-mode)
  (general-define-key
   :keymaps '(company-active-map company-search-map company-filter-map)
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "C-h" #'company-show-doc-buffer)
  (diminish 'company-mode))

(provide 'setup-company)
