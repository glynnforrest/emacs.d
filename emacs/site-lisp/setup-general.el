(require 'setup-flyspell)

(require 'ace-jump-mode)

;; Highlight merge conflicts
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge)

;; Automatically create directories when creating a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


;; Load ssh credentials from keychain, even if keychain was called
;; after emacs startup
(require 'keychain-environment)
(define-key evil-normal-state-map ",k" (lambda ()
                                         (interactive)
                                         (keychain-refresh-environment)
                                         (message "Keychain environment refreshed.")))

;; refresh the current major mode
(define-key global-map (kbd "<f6>") (lambda ()
                                      (interactive)
                                      (call-interactively major-mode)))

(provide 'setup-general)
