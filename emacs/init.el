(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'setup-core)
(require 'setup-evil)
(require 'setup-keys)

(require 'setup-appearance)
(require 'setup-git)
(require 'setup-whitespace)
(require 'setup-ivy)
(require 'setup-org)
(require 'setup-projects)
(require 'setup-defuns)
(require 'setup-yasnippet)
(require 'setup-css)
(require 'setup-js)
(require 'setup-which-key)
(require 'setup-php)
(require 'setup-web-mode)
(require 'setup-saltstack)
(require 'setup-markdown)
(require 'setup-flycheck)
(require 'setup-coffeescript)
(require 'setup-ace-link)
(require 'setup-emmet)
(require 'setup-elisp)
(require 'setup-yaml)
(require 'setup-sql)
(require 'setup-apache)

;; Load personal config if available, like usernames and passwords
(require 'setup-personal nil t)

;; (defvar required-packages nil "A list of required packages for this emacs configuration.")

;; (setq required-packages
;;       '(
;;         ace-jump-mode
;;         ag
;;         auto-complete
;;         autopair
;;         diminish
;;         dired+
;;         emamux
;;         epl
;;         exec-path-from-shell
;;         helm
;;         helm-css-scss
;;         helm-dash
;;         impatient-mode
;;         keychain-environment
;;         multiple-cursors
;;         paredit
;;         paredit-everywhere
;;         php-eldoc
;;         smart-mode-line
;;         smartparens
;;         wgrep
;;         wgrep-ag
;;         ))

;; (defun gf/install-required-packages ()
;;   "Ensure required packages are installed."
;;   (interactive)
;;   (dolist (p required-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p)))
;;   (message (format "%s required packages installed." (length required-packages))))

;; (gf/install-required-packages)

;; (defun gf/package-deps (package)
;;   "Get the dependencies of a package."
;;   (let* ((pkg (cadr (assq package package-alist)))
;;          (deps (if (package-desc-p pkg)
;;                      (package-desc-reqs pkg)
;;                    nil)))
;;     (mapcar (lambda (d) (car d)) deps)))

;; (defun flatten (list)
;;   (cond
;;    ((null list) nil)
;;    ((atom list) (list list))
;;    (t (append (flatten (car list)) (flatten (cdr list))))))

;; (defun gf/required-packages-and-deps ()
;;   "Get the list of required packages and their dependencies."
;;   (remove-duplicates
;;    (append required-packages
;;            (flatten (mapcar 'gf/package-deps required-packages)))))

;; (defun gf/orphan-packages ()
;;   "Get a list of installed packages not explicitly required."
;;   (let ((installed (remove-if-not (lambda (p) (package-installed-p p)) package-activated-list)))
;;     (remove-duplicates (set-difference installed (gf/required-packages-and-deps)))))

;; (defun gf/delete-orphan-packages ()
;;   "Delete installed packages not explicitly required."
;;   (interactive)
;;   (let ((orphans (gf/orphan-packages)))
;;     (dolist (pkg orphans)
;;         (package-delete (cadr (assq pkg package-alist))))
;;     (message (format "Deleted %s orphan packages." (length orphans)))))

;; ;; Make sure stuff installed via homebrew is available
;; (push "/usr/local/bin" exec-path)

;; (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize))


;; (setq plugins-dir (expand-file-name "plugins" user-emacs-directory))

;; (let ((default-directory plugins-dir))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'load-path site-lisp-dir)

;; Load custom settings
;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)


;; Clearly necessary
;; (require 'modes)
;; (require 'setup-autocomplete)
;; (require 'setup-general)
;; (require 'setup-programming)
;; (require 'setup-helm)
;; (require 'defuns)

;; (require 'help-mode)
;; (require 'setup-multiple-cursors)

;; (require 'setup-search)
;; (require 'setup-diminish)
;; (require 'mappings)

;; Mac specific configuration
;; (setq is-mac (equal system-type 'darwin))
;; (when is-mac (require 'setup-mac))

;; (setq tramp-default-method "ssh")
