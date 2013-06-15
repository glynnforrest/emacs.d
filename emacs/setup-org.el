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
(setq org-listen-read-watch-file (concat org-directory "topics/listen-read-watch.org"))

(setq org-files (file-expand-wildcards (concat org-directory "*/*.org")))
(setq org-refile-targets
	  '((org-files :maxlevel . 1)
		(nil :maxlevel . 1)))

(defun gf/org-refile-files-first ()
  "Choose an org file to file in, then pick the node. This prevents
  emacs opening all of the refile targets at once."
  (interactive)
  (let ((file (list (ido-completing-read "Refile to:" org-files))))
	(let ((org-refile-targets `((,file :maxlevel . 1))))
	  (org-refile))))

(setq org-agenda-files (list org-default-notes-file org-listen-read-watch-file))

(define-key global-map (kbd "M-n") 'org-capture)
(define-key global-map (kbd "M-N") (lambda()
                                     (interactive)
                                     (find-file org-default-notes-file)
                                     ))
(define-key global-map (kbd "C-c n") (lambda ()
									   (interactive)
									   (gf/find-file-in-directory org-directory)))
(evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-todo)
(evil-declare-key 'normal org-mode-map (kbd "C-m") 'gf/org-refile-files-first)
(evil-declare-key 'visual org-mode-map (kbd "C-m") 'gf/org-refile-files-first)
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
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Vim style navigation
(define-key org-mode-map (kbd "C-c h") 'outline-up-heading)
(define-key org-mode-map (kbd "C-c j") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-c k") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c g") 'gf/org-end-of-section)

(defun gf/org-end-of-section ()
  "Move to the last line of the current section."
										 (interactive)
										 (re-search-backward "^\* ")
										 (org-forward-element 1)
										 (previous-line 1))
(defun gf/evil-org-beginning-of-line ()
  "Move to the beginning of the line in an org-mode file, ignoring
TODO keywords, stars and list indicators."
 (interactive)
 (beginning-of-line)
 (if (looking-at-p " ") (evil-forward-word-begin))
 (if (looking-at-p "*") (evil-forward-word-begin))
 (if (looking-at-p "TODO\\|DONE\\|WAITING") (evil-forward-word-begin)))

(setq org-todo-keywords
       '((sequence "TODO" "DONE" "WAITING")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#dc322f" :weight bold)
              ("DONE" :foreground "forest green" :weight bold :strike-through t)
              ("WAITING" :foreground "#89BDFF" :weight bold))))

(setq org-log-done t)

(evil-declare-key 'normal org-mode-map "^" 'gf/evil-org-beginning-of-line)
(evil-declare-key 'normal org-mode-map "I"
  (lambda ()
    (interactive)
    (gf/evil-org-beginning-of-line)
    (evil-insert 1)
    ))

(evil-declare-key 'normal org-mode-map ",N" 'org-narrow-to-subtree)

(evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-display-inline-images)
(evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-remove-inline-images)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?" :prepend t)
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?")
        ("T" "Project Todo" entry (file+headline org-current-project-file "Tasks")
         "* TODO %?" :prepend t)
        ("N" "Project Note" entry (file+headline org-current-project-file "Notes")
         "* %?")
        ("l" "Listen" entry (file+headline org-listen-read-watch-file "Listen")
         "* %?")
        ("r" "Read" entry (file+headline org-listen-read-watch-file "Read")
         "* %?")
        ("w" "Watch" entry (file+headline org-listen-read-watch-file "Watch")
         "* %?")
        ("u" "Unsorted" entry (file+headline org-default-notes-file "Unsorted")
         "* %?")
        ))

;; Behaviour for capturing notes using make-capture-frame
 (defadvice org-capture-finalize
  (after delete-capture-frame activate)
   "Advise capture-finalize to close the frame"
   (if (equal "capture" (frame-parameter nil 'name))
       (delete-frame)))

 (defadvice org-capture-destroy
  (after delete-capture-frame activate)
   "Advise capture-destroy to close the frame"
   (if (equal "capture" (frame-parameter nil 'name))
       (delete-frame)))

(defadvice org-switch-to-buffer-other-window
  (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

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
(define-key org-mode-map (kbd "M-<up>") 'elscreen-create)
(define-key org-mode-map (kbd "M-<down>") 'elscreen-kill)
(define-key org-mode-map (kbd "M-<right>") 'elscreen-next)
(define-key org-mode-map (kbd "M-<left>") 'elscreen-previous)

(define-key org-mode-map (kbd "C-c C-n") 'gf/new-code-project)
(define-key org-mode-map (kbd "C-c C-o") 'gf/open-code-project)

(provide 'setup-org)
