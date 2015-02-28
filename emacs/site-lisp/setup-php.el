(require 'php-mode)
(require 'web-mode)
(require 's)
(require 'php-auto-yasnippets)

;; Install refactor tool with `composer global require qafoolabs/php-refactoring-browser`
(require 'php-refactor-mode)
(setq php-refactor-command (expand-file-name "~/.composer/vendor/bin/refactor"))
(add-hook 'php-mode-hook 'php-refactor-mode)

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

(defun gf/php-class-candidates ()
  "Get a list of available PHP classes in the current projectile project."
  (interactive)
  (split-string (shell-command-to-string
                 (concat "~/.emacs.d/bin/php_class_finder.php " (projectile-project-root))) "\n" t))

(defun gf/php-refresh-class-candidates ()
  "Refresh the list of available PHP classes in the current projectile project."
  (interactive)
  (start-process-shell-command "php-class-candidates" nil (concat "~/.emacs.d/bin/php_class_finder.php " (projectile-project-root) " refresh"))
  (message (format "Refreshing class candidates for %s" (projectile-project-root))))

(defun gf/php-insert-use-class ()
  "Add a class to the use declarations in the current file."
  (interactive)
  (save-excursion
    (let ((class (helm-comp-read
                  "Class: "
                  (gf/php-class-candidates)
                  :must-match t
                  )))
      (gf/php-go-to-last-use-statement)
      (end-of-line)
      (newline)
      (insert (concat "use " class ";")))))

(defun gf/php-insert-class ()
  "Insert a class name in the current projectile project."
  (interactive)
    (let ((class (helm-comp-read
                  "Class: "
                  (gf/php-class-candidates)
                  :must-match t
                  )))
      (insert class)))

(defun gf/php-current-file-namespace ()
  "Get a suitable namespace for the current file."
  (interactive)
  (if (s-contains? "src" (buffer-file-name))
      (gf/php-namespace-from-path buffer-file-name "src")

    (if (s-contains? "tests" (buffer-file-name))
        (gf/php-namespace-from-path buffer-file-name "tests")

      (if (s-contains? "app" (buffer-file-name))
          (gf/php-namespace-from-path buffer-file-name "app")))))

(defun gf/php-namespace-from-path (path substr)
  "Extract a namespace from a path name that contains `substr`."
  (let ((namespace-file
         (s-right (- (length path)
                     (s-index-of substr path)
                     (+ 1 (length substr)))
                  path)))
    (s-join "\\" (butlast (s-split "/" namespace-file)))))

(defun gf/evil-open-below-docblock (count)
  "Same as `evil-open-below`, but insert * if in a docblock."
  (interactive "p")
  (evil-first-non-blank)
  (if (looking-at-p "*")
      (progn
        (evil-open-below count)
        (insert "* "))
    (evil-open-below count)))

(evil-declare-key 'normal php-mode-map "o" 'gf/evil-open-below-docblock)

(defun gf/php-cleanup-style ()
  "Cleanup the style of the current php file with php-cs-fixer."
  (interactive)
  (save-buffer)
  (shell-command (concat "php-cs-fixer fix " (buffer-file-name)))
  (let ((point (point)))
    (revert-buffer t t)
    (goto-char point)))

(defun gf/php-insert-neptune-service ()
  "Insert a service name for the current neptune php project."
  (interactive)
  (let ((service (helm-comp-read
                  "Service: "
                  (gf/php-neptune-service-candidates)
                  :must-match t
                  )))
    (insert service)))

(defun gf/php-neptune-service-candidates ()
  "Get services for the current neptune php project."
  (interactive)
  (split-string (shell-command-to-string
                 (concat "cd " (projectile-project-root) " && php neptune service:list -N" )) "\n" t))

(evil-declare-key 'normal php-mode-map ",+" 'gf/php-cleanup-style)

;; Use M-j to open a line below when in insert mode

(setq php-auto-yasnippet-php-program (concat plugins-dir "/php-auto-yasnippets/Create-PHP-YASnippet.php"))
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(evil-declare-key 'normal php-mode-map ",z" 'gf/toggle-php-web-mode)
(evil-declare-key 'normal web-mode-map ",z" 'gf/toggle-php-web-mode)
(define-key php-mode-map (kbd "C-c i") 'gf/php-insert-use-class)
(define-key php-mode-map (kbd "C-c I") 'gf/php-insert-class)
(define-key php-mode-map (kbd "C-c s") 'gf/php-insert-neptune-service)

(define-key php-mode-map (kbd "M-q") 'gf/quit-other-window)

(provide 'setup-php)
