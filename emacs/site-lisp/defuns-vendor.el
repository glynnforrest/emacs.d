;; Functions copied or adapted from elsewhere

;; https://github.com/syl20bnr/spacemacs
(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;; https://github.com/magnars/.emacs.d
(defun delete-current-buffer-file ()
  "Delete the file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename)
        (kill-buffer buffer)
                (message "Deleted '%s'" filename)))))

;; https://github.com/magnars/.emacs.d
;; Adapted to support directory creation
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
            (new-directory (s-join "/" (butlast (s-split "/" new-name)))))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (if (not (file-directory-p new-directory))
              (if (file-exists-p new-directory)
                  (error "Target directory '%s' is a file!" new-directory)
                (mkdir new-directory t)))
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name new-name))))))

(provide 'defuns-vendor)
