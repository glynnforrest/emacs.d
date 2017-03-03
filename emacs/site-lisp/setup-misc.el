(defun gf/maybe-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode t))))

(add-hook 'find-file-hook 'gf/maybe-smerge)

(provide 'setup-misc)
