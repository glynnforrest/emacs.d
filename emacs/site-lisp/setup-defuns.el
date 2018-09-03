(require 'defuns-vendor)

(defun buffer-file-name-body ()
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

(defun gf/face-at-point ()
  "Get the name of the face at point."
  (interactive)
  (message
   (symbol-name (get-char-property (point) 'face))))

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

(provide 'setup-defuns)
