(use-package edit-server
  :config
  (unless (process-status "edit-server")
    (setq edit-server-new-frame t)
    (edit-server-start)))

(provide 'setup-edit-server)