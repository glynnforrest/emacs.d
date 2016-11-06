(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"

   "gs" 'magit-status
   "w" 'save-buffer

   "so" 'swiper

   "TAB" 'previous-buffer
  ))

(provide 'setup-keys)
