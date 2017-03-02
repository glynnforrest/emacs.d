(use-package helm-dash :ensure t
  :config

  (defvar helm-dash-required-docsets '() "A list of required helm-dash-docsets")

  (setq helm-dash-required-docsets
        '(
          Ansible
          Apache_HTTP_Server
          BackboneJS
          Bash
          CSS
          Font_Awesome
          HTML
          Haskell
          Jade
          JavaScript
          LaTeX
          Markdown
          NodeJS
          PHP
          PHPUnit
          SaltStack
          Symfony
          Twig
          Vagrant
          jQuery
          ))

  (defun gf/helm-dash-install-docsets ()
    "Install required docsets"
    (interactive)
    (dolist (doc (mapcar 'symbol-name helm-dash-required-docsets))
      (when (not (member doc (helm-dash-installed-docsets)))
        (message (format "Installing helm-dash docset '%s'" doc))
        (helm-dash-install-docset doc))))

  (defun gf/helm-dash-upgrade-docsets ()
    "Upgrade installed docsets"
    (interactive)
    (dolist (doc (helm-dash-installed-docsets))
      (message (format "Upgrading helm-dash docset '%s'" doc))
      (helm-dash-update-docset doc)))

  ;; By default, no docsets are enabled.
  (setq helm-dash-common-docsets nil))

(provide 'setup-helm-dash)

;; (defun helm-dash-js ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("JavaScript" "BackboneJS" "jQuery")))
;; (add-hook 'js2-mode-hook 'helm-dash-js)

;; (defun helm-dash-html ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Html" "Font_Awesome")))
;; (add-hook 'html-mode-hook 'helm-dash-html)
;; (add-hook 'web-mode-hook 'helm-dash-html)

;; (defun helm-dash-css ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CSS")))
;; (add-hook 'css-mode-hook 'helm-dash-css)

;; (defun helm-dash-shell ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Bash")))
;; (add-hook 'sh-mode-hook 'helm-dash-shell)
