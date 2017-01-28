(use-package ivy :ensure t
  :diminish ""
  :config
  (ivy-mode t)

  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
    '((t   . ivy--regex-ignore-order))))

(use-package counsel :ensure t)

(use-package swiper :ensure t)

(use-package smex :ensure t)

(provide 'setup-ivy)
