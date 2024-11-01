(defun gf/buffer-file-name-body ()
  (if (buffer-file-name)
      (car (split-string (file-name-nondirectory (buffer-file-name)) "\\."))))

(defun gf/filename-extension (filename)
  "Get the extension from a file path."
  (car (last (s-split "\\." filename))))

(defun gf/in-file-type (extension)
  "Return t if the extension of the current buffer file matches
EXTENSION. Only the last extension of the file is considered."
  (equal extension (gf/filename-extension (buffer-file-name))))

(defun gf/candidates-from-command (command)
  "Get a list of candidates from running a command in the projectile root."
  (split-string (shell-command-to-string
                 (concat "cd " (projectile-project-root) " && " command)) "\n" t))

(defun gf/open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun gf/open-personal-file ()
  (interactive)
  (find-file "~/.emacs.d/site-lisp/setup-personal.el"))

(defun gf/find-emacs-d-file ()
  (interactive)
  (projectile-find-file-in-directory user-emacs-directory))

(defun gf/open-zshrc ()
  (interactive)
  (find-file "~/.zshrc"))

(defun gf/open-zshrc-local ()
  (interactive)
  (find-file "~/.zshrc.local"))

(defun gf/open-dotenv-file ()
  (interactive)
  (when (projectile-project-root)
    (find-file (concat (projectile-project-root) "/.env.local"))))

(defun gf/save-buffers-kill-emacs-no-prompt ()
  "Save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun gf/close-buffer-other-window ()
  "Closes the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))

(defun gf/props-at-point ()
  "Get the properties of the text at point. This excludes any overlays
such as hl-line."
  (interactive)
  (message
   (format "%S" (text-properties-at (point)))))

(defun gf/refresh-major-mode ()
  "Refresh the major mode of the current buffer."
  (interactive)
  (call-interactively major-mode)
  (message (format "Refreshed %s" major-mode)))

(defun gf/switch-to-scratch-buffer ()
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

(defun gf/switch-to-messages-buffer ()
  "Switch to the messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defvar gf/url-regex-string "https?:\/\/[-a-z0-9\.\/_\?=%&]+")

(defun gf/open-url-from-buffer ()
  "Prompt to open one of the urls in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((urls ()))
      (while (re-search-forward gf/url-regex-string nil t)
        (let ((url (match-string-no-properties 0)))
          (add-to-list 'urls url)
          ))
      (let ((url (completing-read "Open url in buffer: " urls nil t)))
        (when url
          (browse-url url))))))

(defun gf/open-recent-url ()
  "Open the url closest behind the current point, for example in an
ERC buffer."
  (interactive)
  (save-excursion
    (re-search-backward gf/url-regex-string nil t)
    (let ((url (match-string-no-properties 0)))
      (when url
        (browse-url url)))))

(defun gf/make-current-file-executable ()
  "Add the executable bit to the current file."
  (interactive)
  (if (buffer-file-name)
      (set-file-modes (buffer-file-name)
                      (logior (file-modes (buffer-file-name)) (logand ?\111 (default-file-modes))))
    (error "Not visiting a file.")))

(defun gf/evil-forward-arg (count)
  "Small wrapper around evil-forward-arg when at the opening bracket."
  (interactive "p")
  (if (looking-at-p "(")
      (forward-char))
  (evil-forward-arg count))

(defun gf/maybe-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode t))))

(defun gf/hexstring (length)
  "Generate a hexadecimal string."
  (let ((hash (md5 (number-to-string (random)))))
            (substring hash 0 length)))

(defmacro gf/link-handler (name key regex transform-match)
  (let ((func-name-get (intern (format "gf/get-%s-link" name)))
        (func-name-open (intern (format "gf/open-%s-link" name)))
        (func-name-copy (intern (format "gf/copy-%s-link" name))))
    `(progn
       (defun ,func-name-get ()
         (let ((match (gf/regex-dwim ,regex)))
           (if match
               (,transform-match match)
             nil)))
       (defun ,func-name-open ()
         (interactive)
         (let ((url (,func-name-get)))
           (if url
               (browse-url url)
             (error (format "no %s match found" ,name)))))
       (defun ,func-name-copy ()
         (interactive)
         (message (,func-name-get)))
       (general-define-key
        :states '(normal visual insert emacs)
        :prefix "SPC"
        :non-normal-prefix "M-SPC"
        ,(concat "g" key) ',func-name-open))))

(defun gf/regex-dwim (regex)
  (save-excursion
    (save-match-data
      (let ((start-position (point)))
        (if (eq major-mode 'org-mode)
            (gf/org-up-to-level 2)
          (if (derived-mode-p 'prog-mode)
              (beginning-of-defun)
            (beginning-of-line)))
        (if (re-search-forward regex start-position t)
            (match-string-no-properties 0))))))


(defun gf/uuid ()
  "Generate a pseudo-random UUID. Just the format, not adhering to any versioned UUID spec."
  (let ((hash (md5 (number-to-string (random)))))
    (format "%s-%s-%s-%s-%s"
            (substring hash 0 8)
            (substring hash 8 12)
            (substring hash 12 16)
            (substring hash 16 20)
            (substring hash 20 32))))

;; from `diary.el' (`diary-ordinal-suffix')
(defun ordinal-suffix (n)
  "Ordinal suffix for N. That is, `st', `nd', `rd', or `th', as appropriate."
  (if (or (memq (% n 100) '(11 12 13)) (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

(defun gf/short-date ()
  "Get a short date for headers etc."
  (format (format-time-string "%a %-d%%s %b %Y") (ordinal-suffix (string-to-number (format-time-string "%-d")))))

(defun gf/normalize-quotes (start end)
  "Replace left and right quotation marks with regular versions"
  (interactive "r")
  (replace-regexp-in-region "[““””]" "\"" start end)
  (replace-regexp-in-region "[‘’]" "'" start end))

(provide 'lib-core)
