(require 'php-mode)
;; (php+-mode-setup)

(add-hook 'php-mode-hook (lambda()
							(setq indent-tabs-mode t)
							(setup-electric-semicolon php-mode-map)
							(if (file-has-doctype) (web-mode))
							))

(defun file-has-doctype ()
  (if (string= (upcase (buffer-substring-no-properties 1 10)) "<!DOCTYPE") t nil))

(defun test-this-or-related-php-file ()
  "Run test-case-run on the current buffer, or the related test case
file if open."
  (interactive)
  (save-buffer)
  (if (or test-case-mode (test-case-mode))
	  (test-case-run)
	(let ((b (concat
			  (car (split-string (buffer-name) "\\."))
			  "Test."
			  (car (cdr (split-string (buffer-name) "\\."))))))
	  (if (get-buffer b)
		  (progn
			(switch-to-buffer-other-window b)
			(save-buffer)
			(test-case-run)
			(other-window 1))
	  (message (format "buffer not found: %s" b))))))

(define-key evil-normal-state-map ",t" 'test-this-or-related-php-file)


(provide 'setup-php)
