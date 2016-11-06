(use-package php-mode :ensure t
  :config

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   :keymaps 'php-mode-map
   "c" 'gf/php-insert-use-class
   "C" 'gf/php-insert-class
   "s" 'gf/php-insert-service)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'php-mode-map
   "C-c l" 'yas/create-php-snippet))

(use-package php-auto-yasnippets :ensure t
  :after php-mode)

;; (defun test-this-or-related-php-file ()
;;   "Run test-case-run on the current buffer, or the related test case
;; file if open."
;;   (interactive)
;;   (save-buffer)
;;   (if (or test-case-mode (test-case-mode))
;;       (test-case-run)
;;     (let ((b (concat
;;               (car (split-string (buffer-name) "\\."))
;;               "Test."
;;               (car (cdr (split-string (buffer-name) "\\."))))))
;;       (if (get-buffer b)
;;           (progn
;;             (switch-to-buffer-other-window b)
;;             (save-buffer)
;;             (test-case-run)
;;             (other-window 1))
;;         (message (format "buffer not found: %s" b))))))

(defun gf/php-go-to-namespace ()
  "Go to the namespace declaration at the top of this file."
  (interactive)
  (beginning-of-buffer)
  (if (not (re-search-forward "^namespace" nil t))
      (error "Namespace declaration not found.")
    (beginning-of-line)))

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
    (let ((class (ivy-read
                  "Class: "
                  (gf/php-class-candidates))))
      (gf/php-go-to-last-use-statement)
      (end-of-line)
      (newline)
      (insert (concat "use " class ";")))))

(defun gf/php-insert-class ()
  "Insert a class name in the current projectile project."
  (interactive)
  (let ((class (ivy-read
                "Class: "
                (gf/php-class-candidates))))
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

(defun gf/php-in-template-p ()
  "Return t if the current php file looks like a template."
  (and (> (point-max) 5)
       (not (equal (buffer-substring-no-properties 1 6) "<?php"))))

(defun gf/php-insert-symfony-service ()
  "Insert a service name for the current symfony project."
  (interactive)
  (let ((service (ivy-read
                  "Service: "
                  (gf/candidates-from-command "php bin/console debug:container | sed -E 's/^ +//g' | cut -d ' ' -f 1"))))
    (insert service)))

(defun gf/php-insert-neptune-service ()
  "Insert a service name for the current neptune php project."
  (interactive)
  (let ((service (ivy-read
                  "Service: "
                  (gf/candidates-from-command "php neptune service:list -N | tail -n +2")
                  :must-match t)))
    (insert service)))

(defun gf/php-insert-service ()
  "Insert a service name for the current php project"
  (interactive)
  (if (gf/php-in-symfony-project-p)
      (gf/php-insert-symfony-service)
    (if (gf/php-in-neptune-project-p)
        (gf/php-insert-neptune-service)
      (message "Not in a symfony or neptune project."))))

(defun gf/php-insert-symfony-route ()
  "Insert a route name for the current symfony project."
  (interactive)
  (let ((candidate (ivy-read
                    "Route: "
                    (gf/candidates-from-command "php bin/console debug:router | sed -E 's/^ +//g' |  cut -d ' ' -f 1 | tail -n +3")
                    :must-match t
                    )))
    (insert candidate)))

(defun gf/php-in-symfony-project-p ()
  "Return t if the current projectile project is a symfony project."
  (or
   (file-exists-p (concat (projectile-project-root) "bin/console"))
   (file-exists-p (concat (projectile-project-root) "application/bin/console"))))

(defun gf/php-in-neptune-project-p ()
  "Return t if the current projectile project is a neptune php project."
  (file-exists-p (concat (projectile-project-root) "neptune")))


(provide 'setup-php)
