(require 'php-mode)
;; (php+-mode-setup)

(add-hook 'php-mode-hook (lambda()
							(setq indent-tabs-mode t)
							(setup-electric-semicolon php-mode-map)
							))


(defun gf/toggle-php-web-mode ()
  "Switch between php-mode and web-mode for the current buffer."
  (interactive)
  (if (equal (symbol-name (buffer-local-value 'major-mode (current-buffer))) "web-mode")
	(php-mode)
	(web-mode)
	))

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

(define-key php-mode-map ",t" 'test-this-or-related-php-file)
(define-key php-mode-map ",q" 'gf/toggle-php-web-mode)
(define-key web-mode-map ",q" 'gf/toggle-php-web-mode)

(provide 'setup-php)
