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
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

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
(require 'setup-comments)
(require 'setup-smartparens)
(require 'setup-try-code)
(require 'setup-rotate-text)
(require 'setup-lilypond)
(require 'setup-shell-script)
(require 'setup-os)
(require 'setup-edit-server)

;; Load personal config if available, like usernames and passwords
(require 'setup-personal nil t)

;; (defvar required-packages nil "A list of required packages for this emacs configuration.")

;; (setq required-packages
;;       '(
;;         ace-jump-mode
;;         ag
;;         auto-complete
;;         dired+
;;         emamux
;;         epl
;;         helm
;;         helm-css-scss
;;         helm-dash
;;         impatient-mode
;;         keychain-environment
;;         multiple-cursors
;;         php-eldoc
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

;; Load custom settings
;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)


;; Clearly necessary
;; (require 'setup-autocomplete)
;; (require 'setup-general)
;; (require 'setup-programming)
;; (require 'setup-helm)
;; (require 'defuns)

;; (require 'setup-multiple-cursors)

;; (require 'setup-search)
;; (require 'mappings)

;; (setq tramp-default-method "ssh")
