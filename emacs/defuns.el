(defun my-kill-emacs ()
  "Save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun convert-to-end-of-sentence ()
  "Change the current character to a full stop and capitalise the next word."
  (interactive)
  (delete-char 1)
  (insert ".")
  (evil-forward-word-begin)
  (evil-invert-char (point) (+ 1 (point))))

(defun open-url-from-buffer ()
  "Open a url with ido, choosing from all of the urls in the current
buffer."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let ((urls ()))
	  (while (re-search-forward "http:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
		(let ((url (match-string-no-properties 0)))
		  (add-to-list 'urls url)
		  ))
	  (let ((url (ido-completing-read "Open url in buffer: " urls nil t)))
		(when url
		  (browse-url url))))))

(defun open-recent-url ()
  "Open the url closest behind the current point, for example in an
ERC buffer."
  (interactive)
  (save-excursion
	(re-search-backward "http:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
	(let ((url (match-string-no-properties 0)))
		(when url
		  (browse-url url)))))

(defun evil-org-beginning-of-line ()
  "Move to the beginning of the line in an org-mode file, ignoring
TODO keywords, stars and list indicators."
 (interactive)
 (beginning-of-line)
 (if (looking-at-p " ") (evil-forward-word-begin))
 (if (looking-at-p "*") (evil-forward-word-begin))
 (if (looking-at-p "TODO\\|DONE\\|WAITING") (evil-forward-word-begin)))

(defun comment-or-uncomment-line ()
  "Comments or uncomments the current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Open recent file: " recentf-list nil t)))
	(when file
	  (find-file file))))

(defun gf-find-file-in-directory (directory prompt)
  "Find a file in DIRECTORY using ido. This function depends on the
`projectile` package."
  (let* ((project-files (projectile-hashify-files
                         (projectile-project-files directory)))
         (file (ido-completing-read prompt
                                    (loop for k being the hash-keys in project-files collect k))))
    (find-file (gethash file project-files))))

(defun split-window-and-move-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-and-move-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun switch-to-scratch-buffer()
  "Switch to the scratch buffer. If the buffer doesn't exist,
create it and write the initial message into it."
  (interactive)
  (let* ((scratch-buffer-name "*scratch*")
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer scratch-buffer)))

(defun my-save-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))

(defun my-eval-print-last-sexp ()
  (interactive)
  (end-of-line)
  (eval-print-last-sexp)
  (evil-insert 1))

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
		 (current-buffer)))

(defun move-line-up-and-indent ()
  (interactive)
  (transpose-lines 1)
  (evil-previous-line 2)
  (indent-for-tab-command))

(defun close-help-buffer ()
  "Closes the help buffer."
  (interactive)
  (kill-buffer "*Help*"))

(defun setup-electric-semicolon (mode-map)
  "Adds mappings for electric semicolon to MODE-MAP.
Press ; for electric-semicolon, C-; to insert a semicolon."
  (evil-declare-key 'insert mode-map ";" 'electric-semicolon)
  (evil-declare-key 'insert mode-map (kbd "C-;") (lambda()
												   (interactive)
												   (insert ";"))))

(defun electric-semicolon ()
  "Inserts a semicolon at the end of the current line if not already there."
  (interactive)
  (let ((beg (point)))
  (end-of-line)
  (if (not (looking-back ";"))
    (insert ";")
	(goto-char beg)
	)))

(defun move-line-down-and-indent ()
  (interactive)
  (evil-next-line 1)
  (transpose-lines 1)
  (evil-previous-line 1)
  (indent-for-tab-command))

(provide 'defuns)
