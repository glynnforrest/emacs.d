(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"

   "w" 'save-buffer

   "TAB" 'previous-buffer
  ))

(provide 'setup-keys)
