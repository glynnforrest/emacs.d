;; Dired mode
(require 'dired)
(require 'dired+)
(add-hook 'dired-mode-hook (lambda ()
							 (interactive)
							 (rename-buffer "*Dired*")
							 ))

;; Up directory fix
(defadvice dired-up-directory (around dired-up-fix activate)
  (interactive)
  (rename-buffer "*Dired-old*")
  ad-do-it
  (previous-buffer)
  (kill-this-buffer)
  )

(evil-declare-key 'normal dired-mode-map ",e" (lambda ()
												(interactive)
												(dired-toggle-read-only)
												(evil-normal-state)
												(evil-forward-char)
												))
(toggle-diredp-find-file-reuse-dir 1)

(provide 'setup-dired)
