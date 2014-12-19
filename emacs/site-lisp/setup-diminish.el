(require 'diminish)
(dolist (mode '(
                abbrev-mode
                auto-complete-mode
                autopair-mode
                eldoc-mode
                elisp-slime-nav-mode
                flycheck-mode
                flyspell-mode
                git-gutter-mode
                helm-mode
                magit-auto-revert-mode
                paredit-everywhere-mode
                paredit-mode
                projectile-mode
                php-refactor-mode
                undo-tree-mode
                yas-minor-mode
                ))
  (diminish mode))

(provide 'setup-diminish)
