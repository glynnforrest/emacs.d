(require 'diminish)
(dolist (mode '(
                abbrev-mode
                auto-complete-mode
                autopair-mode
                eldoc-mode
                flycheck-mode
                flyspell-mode
                git-gutter-mode
                helm-mode
                paredit-everywhere-mode
                paredit-mode
                projectile-mode
                php-refactor-mode
                undo-tree-mode
                yas-minor-mode
                ))
  (diminish mode))

(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

(provide 'setup-diminish)
