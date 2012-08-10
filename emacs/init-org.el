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
(setq org-default-notes-file (concat "~/Notes/dates/"(downcase (format-time-string "%Y-%B.org"))))

(define-key global-map (kbd "M-m") 'org-capture)
(define-key global-map (kbd "M-M") (lambda()
                                     (interactive)
                                     (find-file org-default-notes-file)
                                     ))
(evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'normal org-mode-map (kbd "C-m") 'org-refile)
(evil-declare-key 'visual org-mode-map "m" 'org-refile)
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

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?")
        ("l" "Linked Todo" entry (file+headline org-default-notes-file "Unsorted")
         "* TODO %?\n%a")
        ("n" "Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?")
        ("h" "Linked Note" entry (file+headline org-default-notes-file "Unsorted")
         "* %?\n%a")))

(provide 'init-org)
