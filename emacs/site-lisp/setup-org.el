(use-package org :ensure t
  :defer t
  :commands (gf/find-current-month-notes-file
             gf/toggle-switch-to-project-org-file)
  :config
  (require 'defuns-org)

  (setq org-directory "~/notes/")
  (setq org-listen-read-watch-file (concat org-directory "topics/listen-read-watch.org"))

  ;; Split up the search string on whitespace
  (setq org-agenda-search-view-always-boolean t)

  (setq org-refile-targets '((nil :maxlevel . 2)))

  (defun gf/org-reload ()
    "Reload the org file for the current month - useful for a long
running emacs instance."
    (interactive)
    (setq gf/current-month-notes-last-visited nil)
    (setq org-files (append (file-expand-wildcards (concat org-directory "*/*.org"))
                            (file-expand-wildcards (concat org-directory "*/*/*.org"))))

    ;; Notes are grouped by month in dates/ for automatic archival.
    ;; At the start of every month, move over notes that are still relevant.
    ;; Agenda files are only used for searching - this setup is
    ;; designed to work without scheduling, tags etc
    (setq org-agenda-files (append
                            (file-expand-wildcards (concat org-directory "dates/*.org"))
                            (file-expand-wildcards (concat org-directory "topics/*.org"))
                            (file-expand-wildcards (concat org-directory "topics/*/*.org"))))
    (setq org-default-notes-file (gf/org-current-month-notes-file)))

  (defun gf/org-current-month-notes-file ()
    "Get the path of the org file for the current month."
    (concat org-directory "dates/"
            (downcase (format-time-string "%Y-%B.org"))))

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
      (cd old-dir)))

  (defun gf/find-current-month-notes-file ()
    "Find the org file for the current month"
    (interactive)
    (find-file org-default-notes-file))

  (defun gf/org-end-of-section ()
    "Move to the last line of the current section."
    (interactive)
    (re-search-backward "^\* ")
    (org-forward-element 1)
    (previous-line 1))

  (defun gf/org-beginning-of-line ()
    "Move to the beginning of the line in an org-mode file, ignoring
TODO keywords, stars and list indicators."
    (interactive)
    (beginning-of-line)
    (if (looking-at-p " ") (evil-forward-word-begin))
    (if (looking-at-p "*") (evil-forward-word-begin))
    (if (looking-at-p "TODO\\|DONE\\|NEXT\\|WAITING\\|CANCELLED") (evil-forward-word-begin)))

  (defun gf/org-insert-beginning-of-line ()
    "Run `gf/org-beginning-of-line` and change to insert state."
    (interactive)
    (gf/org-beginning-of-line)
    (evil-insert-state t))

  (defun gf/org-change-line ()
    "Change the current line, keeping any org headers and todo titles."
    (interactive)
    (org-show-subtree)
    (end-of-line)
    (let ((eol (point)))
      (gf/org-beginning-of-line)
      (evil-change (point) eol)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "|" "CANCELLED(c)")))

  (setq org-log-done t)

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "#dc322f" :weight bold)
                ("DONE" :foreground "forest green" :weight bold :strike-through t)
                ("WAITING" :foreground "#89BDFF" :weight bold))))

  (setq org-enforce-todo-dependencies t ; can't close without subtasks being done
        org-use-fast-todo-selection t)

  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  (setq org-capture-templates
        '(("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?")
          ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?" :prepend t)
          ("z" "Project note" entry (function gf/org-select-project-file-header)
           "* %?")
          ("x" "Project todo" entry (function gf/org-select-project-file-header)
           "* TODO %?")
          ("c" "Other project note" entry (function gf/org-select-other-project-file-header)
           "* %?")
          ("v" "Other project todo" entry (function gf/org-select-other-project-file-header)
           "* TODO %?")
          ;; ("s" "Someday" entry (file+headline org-someday-file "Notes")
          ;;  "* %?")
          ("l" "Listen" entry (file+headline org-listen-read-watch-file "Listen")
           "* %?")
          ("r" "Read" entry (file+headline org-listen-read-watch-file "Read")
           "* %?")
          ("w" "Watch" entry (file+headline org-listen-read-watch-file "Watch")
           "* %?")
          ("p" "Play" entry (file+headline org-listen-read-watch-file "Play")
           "* %?")))

  (defun gf/org-make-capture-frame ()
    "Make a new frame for using org-capture."
    (interactive)
    (make-frame '((name . "capture") (width . 80) (height . 20)))
    (select-frame-by-name "capture")
    (org-capture))

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

(defvar gf/org-months '("january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december"))

(defun gf/org-calculate-month-file-offset (filename offset)
  "Calculate the name of the neighbouring month file to FILENAME.

FILENAME is expected to be of the form <year>-<monthname>, e.g. 2016-november.

OFFSET is t for next month, or nil for previous month."
  (let* ((pieces (split-string filename "-"))
         (year (string-to-number (car pieces)))
         (month (cadr pieces))
         (month-number (position month gf/org-months :test 'equal)))
    (if offset
        (if (eq month-number 11)
            (concat (number-to-string (+ year 1)) "-january")
          (concat (number-to-string year) "-" (nth (+ month-number 1) gf/org-months)))
      (if (eq month-number 0)
          (concat (number-to-string (- year 1)) "-december")
        (concat (number-to-string year) "-" (nth (- month-number 1) gf/org-months))))))

(defun gf/org-go-to-next-month ()
  "Go to the next month org file."
  (interactive)
  (find-file (concat org-directory "dates/" (gf/org-calculate-month-file-offset (buffer-file-name-body) t) ".org")))

(defun gf/org-go-to-previous-month ()
  "Go to the previous month org file."
  (interactive)
  (find-file (concat org-directory "dates/" (gf/org-calculate-month-file-offset (buffer-file-name-body) nil) ".org")))

(use-package time-ext :ensure t)

(defun gf/org-previous-month-name-from-filename (filename)
  (when (stringp filename)
    (let ((index (position (cadr (split-string filename "-")) gf/org-months :test 'equal)))
      (capitalize
       (if (eq 0 index) "december" (nth (- index 1) gf/org-months))))))

(defun gf/org-generate-weekly-reviews (filename)
  "Generate org task headers for weekly reviews for the given notes file FILENAME.

FILENAME is expected to be of the form <year>-<monthname>, e.g. 2016-november."
  (when (stringp filename)
    (string-join
     (mapcar
      (lambda(i)
        (concat
         "** TODO Review of week beginning " i "\n"
         "*** TODO Type up written notes\n"
         "*** TODO Import notes from email\n"
         "*** TODO Log work, timesheets, goal tracking\n"
         "*** TODO Cleanup downloads folder and home directory\n"
         "*** TODO Record taxable income and expenses, save 30% of income\n"
         "*** TODO Backups"
         ))
      (mapcar (lambda (date)
                (format "%d%s of %s" (nth 3 date) (ordinal-suffix (nth 3 date)) (capitalize (nth (- (nth 4 date) 1) gf/org-months))))
              (gf/org-week-starts filename)))
     "\n")))

(defun gf/org-week-starts (filename)
  "Get the mondays from weeks that intersect a given month for notes file FILENAME.

This will also include a monday from the previous month, if the week
intersects with the current month.

Dates are returned in the style from `decode-time'."
  (when (stringp filename)
    (let* ((pieces (split-string filename "-"))
           (year (string-to-number (car pieces)))
           (month (cadr pieces))
           (month-number (+ 1 (position month gf/org-months :test 'equal)))
           (dates '()))
      (progn
        ;; get all sundays in the month
        (let ((date (decode-time (next-nweek 0 (parse-time-string (format "1:00:00 %d %s %d" 1 month year))))))
          (while (eq month-number (nth 4 date))
            (setq dates (append dates (list date)))
            (setq date (decode-time (time-add-day date 7)))))

        ;; account for the 1st being a Sunday, and skipped by next-nweek (first will be 8th)
        (when (eq 8 (nth 3 (car dates)))
          (push (decode-time (time-add-day (car dates) -7)) dates))

        ;; subtract 6 days to get the week beginning for every sunday
        (mapcar (lambda (date) (decode-time (time-add-day date -6))) dates)))))

;; from `diary.el' (`diary-ordinal-suffix')
(defun ordinal-suffix (n)
  "Ordinal suffix for N. That is, `st', `nd', `rd', or `th', as appropriate."
  (if (or (memq (% n 100) '(11 12 13)) (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

(defun gf/find-org-info-file ()
  "Open info.org, full of delicious secrets."
  (interactive)
  (find-file (concat org-directory "topics/info.org")))

  ;; (defun gf/create-project-branch-from-org-heading ()
  ;;   "Create a git feature branch for the current org heading. The project is guessed from the current org file.")

  (gf/org-reload)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'org-mode-map
   :prefix gf/major-mode-leader-key
   :non-normal-prefix gf/major-mode-non-normal-leader-key
   "r" 'gf/org-refile-files-first
   "R" 'org-refile
   "." 'gf/org-go-to-next-month
   "," 'gf/org-go-to-previous-month)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'org-mode-map
   "a" 'gf/org-select-top-level-header-or-all
   "A" 'gf/org-select-next-task)

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
   "^" 'gf/org-beginning-of-line
   "I" 'gf/org-insert-beginning-of-line
   "S" 'gf/org-change-line
   "t" 'org-todo
   "TAB" 'org-cycle))

(provide 'setup-org)
