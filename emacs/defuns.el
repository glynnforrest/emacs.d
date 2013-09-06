(defun my-kill-emacs ()
  "Save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun convert-to-end-of-sentence ()
  "Change the next comma to a full stop and capitalise the next word."
  (interactive)
  (if (not (looking-at-p ","))
      (evil-find-char 1 (string-to-char ",")))
  (delete-char 1)
  (insert ".")
  (evil-forward-word-begin)
  (evil-upcase (point) (+ 1 (point))))

(defun convert-end-of-sentence-to-comma ()
  "Change the next full stop to a comma and lowercase the next word."
  (interactive)
  (if (not (looking-at-p "\\."))
      (evil-find-char 1 (string-to-char ".")))
  (delete-char 1)
  (insert ",")
  (evil-forward-word-begin)
  (if (not (looking-at-p "I"))
      (evil-downcase (point) (+ 1 (point)))))


(defun open-url-from-buffer ()
  "Open a url with ido, choosing from all of the urls in the current
buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((urls ()))
      (while (re-search-forward "https?:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
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
    (re-search-backward "http?:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
    (let ((url (match-string-no-properties 0)))
      (when url
        (browse-url url)))))

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

(defun gf/find-file-in-directory (directory)
  "Hacky function to find a file in DIRECTORY using ido. This depends
on the `projectile` package."
  (interactive)
  (switch-to-scratch-buffer)
  (cd directory)
  (projectile-find-file nil)
  )

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

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun quit-other-window ()
  "Closes the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))

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

(defun make-capture-frame ()
  "Make a new frame for using org-capture."
  (interactive)
  (make-frame '((name . "capture") (width . 80) (height . 20)))
  (select-frame-by-name "capture")
  (org-capture))

(defun fix-double-capital()
  "Go back to last occurence of a 'double capital' at start of word and correct."
  (interactive)
  (save-excursion
	(re-search-backward "\\b[[:upper:]]\\{2\\}"
						nil
						(message "No double capital found!"))
	(forward-char)
	(set-mark-command nil)
	(forward-char)
	(setq deactivate-mark nil)
	(call-interactively 'downcase-region)))

(require 'rotate-text)

(defun clever-rotate-text ()
  "Wrapper to rotate-text that will try the start of the line as well
as the current word."
  (interactive)
  (if (not (condition-case nil
               (rotate-text 1)
             (error nil)))
      (save-excursion
        (evil-first-non-blank)
        (rotate-text 1)
        )))

(defun clever-rotate-text-backward ()
  "Wrapper to rotate-text-backward that will try the start of the
line as well as the current word."
  (interactive)
  (if (not (condition-case nil
               (rotate-text-backward 1)
             (error nil)))
      (save-excursion
        (evil-first-non-blank)
        (rotate-text-backward 1)
        )))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(provide 'defuns)
