(require 'php-mode)
(require 'web-mode)
;; (php+-mode-setup)

;; use tabs for php
(add-hook 'php-mode-hook (lambda()
						   (setq indent-tabs-mode t)
						   (flymake-mode)
						   (setup-electric-semicolon php-mode-map)
						   ))

(setq web-mode-disable-auto-pairing nil)
(setq web-mode-disable-css-colorization nil)


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

(defun gf/php-go-to-namespace ()
  "Go to the namespace declaration at the top of this file."
  (interactive)
  (beginning-of-buffer)
  (if (not (re-search-forward "^namespace" nil t))
	  (error "Namespace declaration not found.")
	(beginning-of-line)
	))

(defun gf/php-go-to-last-use-statement ()
  "Go to the last use statement at the top of this file."
  (interactive)
  (end-of-buffer)
  (if (not (re-search-backward "^use" nil t))
	  (gf/php-go-to-namespace)
	)
  (recenter))

(defun gf/php-insert-use-class ()
  "Add a class to the use declarations in the current file."
  (interactive)
  (save-excursion
	(gf/php-go-to-namespace)
	(let ((file (read-string "Class Name: ")))
	  (gf/php-go-to-last-use-statement)
	  (end-of-line)
	  (newline)
	  (insert (concat "use " file ";"))
	  )))

(defun gf/php-sort-use-statements ()
  "Sort use statements in the current file."
  (interactive)
  )

(evil-declare-key 'normal php-mode-map ",t" 'test-this-or-related-php-file)
(evil-declare-key 'normal php-mode-map ",q" 'gf/toggle-php-web-mode)
(evil-declare-key 'normal web-mode-map ",q" 'gf/toggle-php-web-mode)

(provide 'setup-php)
