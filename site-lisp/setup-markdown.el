(eval-when-compile (require 'use-package))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t)

  (general-define-key
   :keymaps 'markdown-mode-map
   "M-h" 'markdown-promote-subtree
   "M-l" 'markdown-demote-subtree
   "M-H" 'markdown-promote
   "M-L" 'markdown-demote
   "M-RET" 'markdown-insert-header-dwim)

  (general-define-key
   :keymaps 'markdown-mode-map
   :states 'normal
   "TAB" 'outline-cycle)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gf/leader-key
   :non-normal-prefix gf/non-normal-leader-key
   :keymaps 'markdown-mode-map
   "gj" 'gf/markdown-open-jira-ticket)

  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'ws-butler-mode))


(defun gf/markdown-jira-ticket ()
  (save-excursion
    (save-match-data
      (markdown-back-to-heading)
      (if (re-search-forward "[A-Z]+-[0-9]+" (line-end-position) t)
          (match-string-no-properties 0)))))


(defun gf/markdown-open-jira-ticket ()
  (interactive)
  (let ((ticket (gf/markdown-jira-ticket)))
    (when ticket
      (browse-url (concat "https://roblox.atlassian.net/browse/" ticket)))))

(provide 'setup-markdown)
