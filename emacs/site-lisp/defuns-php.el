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

(defun gf/php-use-class (classname)
  "Insert CLASSNAME after the last use statement at the top of this file."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (not (re-search-backward "^use" nil t))
        (progn
          (goto-char (point-min))
          (if (not (re-search-forward "^namespace" nil t))
              (error "Namespace declaration not found."))
          (end-of-line)
          (newline)))
    (end-of-line)
    (newline)
    (insert (concat "use " classname ";"))))

(defun gf/php-use-trait (classname)
  "Use CLASSNAME as a trait in the current file."
  (interactive)
  (gf/php-use-class classname)
  (save-excursion
    (goto-char (point-max))
    (if (not (re-search-backward "^    use" nil t))
        (progn
          (goto-char (point-min))
          (if (not (re-search-forward "^class" nil t))
              (error "Unable to detect where to add a trait use statement."))
          (forward-line)
          (when (not (looking-at-p "{"))
            (forward-line -1))
          (end-of-line)
          (newline)
          (forward-line -1)))
    (end-of-line)
    (newline)
    (let ((classbasename (car (last (split-string classname "\\\\")))))
      (insert (concat "    use " classbasename ";")))))

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

(defun gf/php-use-class-select ()
  "Add a class to the use declarations in the current file."
  (interactive)
  (gf/php-use-class (helm-comp-read
                     "Class: "
                     (gf/php-class-candidates))))

(defun gf/php-use-trait-select ()
  "Use a trait in the current class."
  (interactive)
  (gf/php-use-trait (helm-comp-read
                     "Trait: "
                     (gf/php-class-candidates))))

(defun gf/php-insert-class ()
  "Insert a class name in the current projectile project."
  (interactive)
  (let ((class (helm-comp-read
                "Class: "
                (gf/php-class-candidates))))
    (insert class)))

(defun gf/php-current-file-namespace ()
  "Get a suitable namespace for the current file."
  (gf/php-file-namespace buffer-file-name))

(defun gf/php-file-namespace (file)
  "Get a suitable namespace for `FILE`."
  (shell-command-to-string
   (concat user-emacs-directory "bin/php_resolve_namespace.php " file)))

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
  (shell-command (concat "php-cs-fixer fix --rules=@Symfony --using-cache=false " (buffer-file-name)))
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
  (let ((service (helm-comp-read
                  "Service: "
                  (gf/candidates-from-command "php bin/console debug:container | sed -E 's/^ +//g' | cut -d ' ' -f 1"))))
    (insert service)))

(defun gf/php-insert-neptune-service ()
  "Insert a service name for the current neptune php project."
  (interactive)
  (let ((service (helm-comp-read
                  "Service: "
                  (gf/candidates-from-command "php neptune service:list -N | tail -n +2"))))
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
  (let ((candidate (helm-comp-read
                    "Route: "
                    (gf/candidates-from-command "php bin/console debug:router | sed -E 's/^ +//g' |  cut -d ' ' -f 1 | tail -n +3"))))
    (insert candidate)))

(defun gf/php-insert-symfony-twig-helper ()
  "Insert a twig function or filter for the current symfony project."
  (interactive)
  (insert (helm-comp-read
           "Twig helper: "
           (gf/candidates-from-command
            "php bin/console debug:twig | grep -oE '\\* [-_a-zA-Z0-9]+' | sed 's/^. //g'"))))

(defun gf/php-in-symfony-project-p ()
  "Return t if the current projectile project is a symfony project."
  (or
   (file-exists-p (concat (projectile-project-root) "bin/console"))
   (file-exists-p (concat (projectile-project-root) "application/bin/console"))))

(defun gf/php-in-neptune-project-p ()
  "Return t if the current projectile project is a neptune php project."
  (file-exists-p (concat (projectile-project-root) "neptune")))

(defun gf/php-show-date-format-help ()
  "Show a read-only buffer in another window with the PHP date format parameters."
  (interactive)
  (with-current-buffer-window "*PHP Date Help*" nil nil
                              (princ "
Time              Date                  Timezone                       Other
------------------------------------------------------------------------------------------------------
a: am/pm          d: Day 01-31          e: UTC, GMT, Atlantic/Azores   B: Swatch 000-999
A: AM/PM          D: Mon-Sun            O: +0200                       c: ISO 8601
g: 12/hr 1-12     F: January-December   P: +02:00                      I: DST ? 1 : 0
G: 24/hr 0-23     j: Day 1-31           T: UTC, GMT, EST               L: Leap year ? 1 : 0
h: 12/hr 01-12    l: Monday-Sunday      Z: TZ offset in seconds        o: ISO-8601 week-numbering year
H: 24/hr 00-23    m: Month 01-12                                       r: RFC 2822
i: Min 00-59      M: Jan-Dec                                           t: Days in month
u: Microseconds   n: Month 1-12                                        U: Seconds since epoch
v: Milliseconds   N: Day 1-7                                           z: Day of year 0-365
                  s: Sec 00-59
                  S: nd, rd, th
                  w: Day 0-6
                  W: Week of year
                  y: Year 00-99
                  Y: YYYY
")))

(provide 'defuns-php)
