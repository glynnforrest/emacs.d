(use-package org :ensure t
  :config

  ;;Notes are grouped by months for automatic archival.
  ;;At the start of every month move over notes that are still relevant.
  (setq org-directory "~/notes/")
  (setq org-listen-read-watch-file (concat org-directory "topics/listen-read-watch.org"))

  (setq org-files (append (file-expand-wildcards (concat org-directory "*/*.org"))
			  (file-expand-wildcards (concat org-directory "*/*/*.org"))))


  ;; Split up the search string on whitespace
  (setq org-agenda-search-view-always-boolean t)


  (defun gf/org-reload ()
    "Reload the org file for the current month - useful for a long
running emacs instance."
    (interactive)
    (setq gf/current-month-notes-last-visited nil)
    ;; Agenda files are only used for searching - my notes are designed to
    ;; work without scheduling, tags etc
    (setq org-agenda-files (append
			    (file-expand-wildcards (concat org-directory "dates/*.org"))
			    (file-expand-wildcards (concat org-directory "topics/*.org"))
			    (file-expand-wildcards (concat org-directory "topics/*/*.org"))))
    (setq org-default-notes-file
	  (concat org-directory "dates/"
		  (downcase (format-time-string "%Y-%B.org")))))

  (gf/org-reload)

  (setq org-refile-targets
	'((nil :maxlevel . 2)))

  (defun gf/org-refile-files-first ()
    "Choose an org file to file in, then pick the node. This prevents
  emacs opening all of the refile targets at once."
    (interactive)
    (let ((file (list (completing-read "Refile to: " org-files nil t))))
      (let ((org-refile-targets `((,file :maxlevel . 1))))
	(org-refile)))
    (org-save-all-org-buffers))

  (defun gf/commit-notes ()
    "Commit all org files to git with the current date and time. New files must be explicitly added - this prevents accidental committing of junk files"
    (interactive)
    (let ((old-dir default-directory))
      (cd org-directory)
      (shell-command (concat "git add -u . && git commit -m \"" (format-time-string "%a %e %b %H:%M:%S\"")))
      (cd old-dir)
      ))

  (defvar gf/current-month-notes-last-visited nil
    "The last date the org file for the current month was opened.")

  (defun gf/find-current-month-notes-file ()
    "Find the org file for the current month"
    (interactive)
    (setq gf/current-month-notes-last-visited (format-time-string "%D"))
    (find-file org-default-notes-file))

  (defun gf/check-current-month-notes-reminder ()
    "Show a reminder message if the current notes file hasn't been visited today."
    (if (not (equal gf/current-month-notes-last-visited (format-time-string "%D")))
	(message (format "Check your notes for today, %s" (format-time-string "%A %e of %B")))))

  (add-hook 'find-file-hook 'gf/check-current-month-notes-reminder)


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
    (if (looking-at-p "TODO\\|DONE\\|NEXT\\|WAITING") (evil-forward-word-begin)))

  (defun gf/org-go-to-next-task ()
    "Go to the first org item in the buffer tagged as `NEXT`."
    (interactive)
    (beginning-of-buffer)
    (re-search-forward "^\\*+ NEXT")
    (gf/evil-org-beginning-of-line))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "WAITING(w)" "|" "CANCELLED(c)")))

  (setq org-log-done t)

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "#dc322f" :weight bold)
		("DONE" :foreground "forest green" :weight bold :strike-through t)
		("WAITING" :foreground "#89BDFF" :weight bold))))

  ;; Make it impossible to complete a task if subtasks are not done
  (setq org-enforce-todo-dependencies t)

  (setq org-use-fast-todo-selection t)


  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %?" :prepend t)
	  ("n" "Note" entry (file+headline org-default-notes-file "Notes")
	   "* %?")
	  ("T" "Project Todo" entry (file+headline gf/current-project-file "Tasks")
	   "* TODO %?" :prepend t)
	  ("N" "Project Note" entry (file+headline gf/current-project-file "Notes")
	   "* %?")
	  ("l" "Listen" entry (file+headline org-listen-read-watch-file "Listen")
	   "* %?")
	  ("r" "Read" entry (file+headline org-listen-read-watch-file "Read")
	   "* %?")
	  ("w" "Watch" entry (file+headline org-listen-read-watch-file "Watch")
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (js . t)
     (lilypond . t)
     (haskell . t)
     (python . t)
     (sh . t)
     ))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (defvar org-projects-dir (expand-file-name  "~/notes/projects"))

  (defvar gf/project-org-file-overrides '()
    "A list of projectile directories and the specified project org file for them.")

  (defun gf/create-org-path (path)
    "Create a name suitable for an org file from the last part of a file
path."
    (let ((last (car (last (split-string (if (equal (substring path -1) "/")
					     (substring path 0 -1) path) "/")))))
      (concat org-projects-dir "/"
	      (downcase
	       (replace-regexp-in-string
		"\\." "-" (if (equal (substring last 0 1) ".")
			      (substring last 1) last)))
	      ".org")))

  (defun gf/resolve-project-org-file ()
    "Get the org file for a project, either using a suitable name
automatically or fetching from gf/project-org-file-overrides."

    )

  (defun gf/project-org-file ()
    "Get the path of the org file for the current project."
    (gf/create-org-path (projectile-project-root)))

  (defun gf/switch-to-project-org-file ()
    "Switch to the org file for the current project."
    (interactive)
    (find-file (gf/project-org-file)))

  (defvar gf/previous-project-buffers (make-hash-table :test 'equal))

  (defun gf/toggle-switch-to-project-org-file ()
    "Alternate between the current buffer and the org file for the
current project."
    (interactive)
    (if (and
	 (string-equal "org-mode" (symbol-name major-mode))
	 (s-contains-p "/notes/" (buffer-file-name)))
	(if (gethash (buffer-file-name) gf/previous-project-buffers)
	    (switch-to-buffer (gethash (buffer-file-name) gf/previous-project-buffers))
	  (error "Previous project buffer not found"))
      (let ((file (gf/project-org-file)))
	(puthash file (current-buffer) gf/previous-project-buffers)
	(find-file file)
	)))

  (defun gf/create-project-branch-from-org-heading ()
    "Create a git feature branch for the current org heading. The project is guessed from the current org file."

    )

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'org-mode-map
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   "r" 'gf/org-refile-files-first
   "R" 'org-refile)

  (general-define-key
   :states '(normal visual insert)
   :keymaps 'org-mode-map
   "M-l" 'org-metaright
   "M-h" 'org-metaleft
   "M-k" 'org-metaup
   "M-j" 'org-metadown
   "M-L" 'org-shiftmetaright
   "M-H" 'org-shiftmetaleft
   "M-K" 'org-shiftmetaup
   "M-J" 'org-shiftmetadown)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "t" 'org-todo
   "TAB" 'org-cycle))

(provide 'setup-org)
