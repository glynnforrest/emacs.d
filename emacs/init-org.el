;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
                            (kbd "M-l") 'org-metaright
                            (kbd "M-h") 'org-metaleft
                            (kbd "M-k") 'org-metaup
                            (kbd "M-j") 'org-metadown
                            (kbd "M-L") 'org-shiftmetaright
                            (kbd "M-H") 'org-shiftmetaleft
                            (kbd "M-K") 'org-shiftmetaup
                            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

;;Notes are grouped by months for automatic archival.
;;At the start of every month move over notes that are still relevant.
(setq org-directory "~/notes/")
(setq org-default-notes-file (concat org-directory "dates/" (downcase (format-time-string "%Y-%B.org"))))
(setq org-files (file-expand-wildcards (concat org-directory "*/*.org")))
(setq org-refile-targets
	  '((org-files :maxlevel . 1)
		(nil :maxlevel . 1)))
(setq org-agenda-files (list org-default-notes-file))

(define-key global-map (kbd "M-n") 'org-capture)
(define-key global-map (kbd "M-N") (lambda()
                                     (interactive)
                                     (find-file org-default-notes-file)
                                     ))
(define-key global-map (kbd "C-M-n") (lambda ()
									   (interactive)
									   (gf-find-file-in-directory org-directory "Find org file: ")))
(evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'normal org-mode-map (kbd "C-m") 'org-refile)
(evil-declare-key 'visual org-mode-map (kbd "C-m") 'org-refile)
(evil-declare-key 'insert org-mode-map (kbd "M-<return>") (lambda()
                                                            (interactive)
                                                            (evil-append-line 1)
                                                            (org-meta-return)
                                                            ))
(evil-declare-key 'normal org-mode-map (kbd "M-<return>") (lambda()
                                                            (interactive)
                                                            (evil-append-line 1)
                                                            (org-meta-return)
                                                            ))
(evil-declare-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
(define-key org-mode-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key org-mode-map (kbd "C-j") 'evil-window-down)
(define-key org-mode-map (kbd "C-k") 'evil-window-up)

;; Vim style navigation
(define-key org-mode-map (kbd "C-c h") 'outline-up-heading)
(define-key org-mode-map (kbd "C-c j") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-c k") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c C-j") 'org-forward-same-level)
(define-key org-mode-map (kbd "C-c C-k") 'org-backward-same-level)

(setq org-todo-keywords
       '((sequence "TODO" "DONE" "WAITING")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#dc322f" :weight bold)
              ("DONE" :foreground "forest green" :weight bold :strike-through t)
              ("WAITING" :foreground "#89BDFF" :weight bold))))

(evil-declare-key 'normal org-mode-map "^" (lambda()
											 (interactive)
											 (beginning-of-line)
											 (if (looking-at-p " ") (evil-forward-word-begin))
											 (if (looking-at-p "*") (evil-forward-word-begin))
											 (if (looking-at-p "TODO\\|DONE\\|WAITING") (evil-forward-word-begin))
											 ))

(evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-display-inline-images)
(evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-remove-inline-images)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?")
        ("l" "Linked Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?\n%a")
        ("n" "Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?")
        ("h" "Linked Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?\n%a")))

;;babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (lilypond . t)
   (sh . t)
   ))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(evil-declare-key 'normal org-mode-map ",e" 'org-ctrl-c-ctrl-c)

(provide 'init-org)
