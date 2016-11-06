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

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

(require 'setup-evil)
(require 'setup-keys)

;; (defvar required-packages nil "A list of required packages for this emacs configuration.")

;; (setq required-packages
;;       '(
;;         ace-jump-mode
;;         ag
;;         apache-mode
;;         auto-complete
;;         autopair
;;         browse-kill-ring
;;         color-theme-sanityinc-tomorrow
;;         coffee-mode
;;         diminish
;;         dired+
;;         el-autoyas
;;         elisp-slime-nav
;;         emamux
;;         emmet-mode
;;         epl
;;         ethan-wspace
;;         evil
;;         evil-args
;;         evil-exchange
;;         evil-leader
;;         evil-jumper
;;         exec-path-from-shell
;;         expand-region
;;         flycheck
;;         git-gutter
;;         git-timemachine
;;         helm
;;         helm-css-scss
;;         helm-dash
;;         helm-ls-git
;;         helm-projectile
;;         helm-swoop
;;         impatient-mode
;;         js-comint
;;         js2-mode
;;         js2-refactor
;;         keychain-environment
;;         magit
;;         markdown-mode
;;         multiple-cursors
;;         org
;;         paredit
;;         paredit-everywhere
;;         php-eldoc
;;         php-mode
;;         php-refactor-mode
;;         projectile
;;         rainbow-delimiters
;;         rainbow-mode
;;         salt-mode
;;         scss-mode
;;         skewer-mode
;;         smart-mode-line
;;         smartparens
;;         sqlup-mode
;;         surround
;;         web-mode
;;         wgrep
;;         wgrep-ag
;;         yaml-mode
;;         yasnippet
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

;; Load personal configurations, like usernames and passwords
;; (require 'personal nil t)

;; Load appearance early to reduce flicker of default emacs
;; (require 'appearance)

;; Clearly necessary
;; (require 'modes)
;; (require 'setup-autocomplete)
;; (require 'setup-general)
;; (require 'setup-whitespace)
;; (require 'setup-programming)
;; (require 'setup-projects)
;; (require 'setup-helm)
;; (require 'setup-yaml)
;; (require 'defuns)

;; (require 'help-mode)
;; (require 'setup-multiple-cursors)

;; (require 'setup-org)
;; (require 'setup-yasnippet)
;; (require 'setup-search)
;; (require 'setup-diminish)
;; (require 'mappings)

;; Mac specific configuration
;; (setq is-mac (equal system-type 'darwin))
;; (when is-mac (require 'setup-mac))

;; (setq tramp-default-method "ssh")
