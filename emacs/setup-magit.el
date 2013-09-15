;; Magit
(require 'magit)
(require 'git-commit-mode)

(evil-declare-key 'normal git-commit-mode-map ",w" 'git-commit-commit)
(define-key magit-mode-map "q" (lambda ()
								 (interactive)
								 (if (get-buffer "*magit-process*")
									 (kill-buffer "*magit-process*"))
								 (if (get-buffer "*magit-edit-log*")
									 (kill-buffer "*magit-edit-log*"))
								 (kill-this-buffer)
								 (delete-window)
								 ))

;; Preview changes without leaving the buffer
(define-key magit-mode-map (kbd "<S-return>") (lambda()
												(interactive)
											  (let ((current-prefix-arg t))
												(magit-visit-item))))

(define-key magit-mode-map (kbd "<return>") (lambda()
											  (interactive)
											  (universal-argument)
											  (let ((current-prefix-arg t))
												(magit-visit-item))
											  (other-window 1)
											  ))

(provide 'setup-magit)
