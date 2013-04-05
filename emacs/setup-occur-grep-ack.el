;; Occur mode
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
		(if (eq mode major-mode)
			(add-to-list 'buffer-mode-matches buf))))
	buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun occur-goto-occurrence-recenter ()
  "Go to the occurrence on the current line and recenter."
  (interactive)
  (occur-mode-goto-occurrence)
  (recenter))

;; Preview occurrences in occur without leaving the buffer
(defun occur-display-occurrence-recenter ()
  "Display the occurrence on the current line in another window and recenter."
  (interactive)
  (occur-goto-occurrence-recenter)
  (other-window 1))

(evil-declare-key 'normal occur-mode-map (kbd "<return>") 'occur-display-occurrence-recenter)
(evil-declare-key 'normal occur-mode-map (kbd "<S-return>") 'occur-goto-occurrence-recenter)

(define-key evil-normal-state-map ",o" (lambda()
										 (interactive)
										 (call-interactively 'occur)
										 (other-window 1)
										 ))

(define-key evil-normal-state-map ",O" (lambda()
										 (interactive)
										 (call-interactively 'multi-occur-in-this-mode)
										 (other-window 1)
										 ))

(evil-declare-key 'normal occur-mode-map ",e" 'occur-edit-mode)
(evil-declare-key 'normal occur-edit-mode-map ",e" 'occur-cease-edit)

;; Grep mode
(defun grep-goto-occurrence-recenter ()
  "Go to the occurrence on the current line and recenter."
  (interactive)
  (compile-goto-error)
  (recenter))

(defun grep-display-occurrence-recenter ()
  "Display the grep result of the current line in another window and recenter."
  (interactive)
  (grep-goto-occurrence-recenter)
  (other-window 1))

(evil-declare-key 'normal grep-mode-map (kbd "<return>") 'grep-display-occurrence-recenter)
(evil-declare-key 'normal grep-mode-map (kbd "<S-return>") 'grep-goto-occurrence-recenter)
(evil-declare-key 'normal ack-and-a-half-mode-map (kbd "<return>") 'grep-display-occurrence-recenter)
(evil-declare-key 'normal ack-and-a-half-mode-map (kbd "<S-return>") 'grep-goto-occurrence-recenter)

(defun gf/narrow-grep-buffer ()
  "Narrow the grep buffer stripping out the really long grep command."
  (interactive)
  (goto-line 5)
  (narrow-to-region (point) (point-max))
  (goto-line 1))

(define-key evil-normal-state-map (kbd "C-c g")
  (lambda()
    (interactive)
    (call-interactively 'projectile-ack)
    (other-window 1)
    (gf/narrow-grep-buffer)
    ))

(require 'wgrep)
(require 'wgrep-ack)
(evil-declare-key 'normal grep-mode-map ",e" 'wgrep-change-to-wgrep-mode)
(evil-declare-key 'normal grep-mode-map ",w" 'wgrep-save-all-buffers)
(evil-declare-key 'normal ack-and-a-half-mode-map ",e" 'wgrep-change-to-wgrep-mode)
(evil-declare-key 'normal ack-and-a-half-mode-map ",w" 'wgrep-save-all-buffers)
(evil-declare-key 'normal wgrep-mode-map ",e" 'wgrep-finish-edit)
(evil-declare-key 'normal wgrep-mode-map ",x" 'wgrep-abort-changes)

(provide 'setup-occur-grep-ack)
