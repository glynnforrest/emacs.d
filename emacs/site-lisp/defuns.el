(defun gf/comma-to-end-of-sentence ()
  "Change the next comma to a full stop and capitalise the next word."
  (interactive)
  (if (not (looking-at-p ","))
      (evil-find-char 1 (string-to-char ",")))
  (delete-char 1)
  (insert ".")
  (evil-forward-word-begin)
  (evil-upcase (point) (+ 1 (point))))

(defun gf/end-of-sentence-to-comma ()
  "Change the next full stop to a comma and lowercase the next word."
  (interactive)
  (if (not (looking-at-p "\\."))
      (evil-find-char 1 (string-to-char ".")))
  (delete-char 1)
  (insert ",")
  (evil-forward-word-begin)
  (if (not (looking-at-p "I"))
      (evil-downcase (point) (+ 1 (point)))))


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

(defun gf/split-window-and-move-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun gf/split-window-and-move-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun gf/switch-to-scratch-buffer()
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

(defun gf/save-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))

(defun gf/eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (if (evil-normal-state-p) (evil-append 1))
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(defun gf/setup-electric-semicolon (mode-map)
  "Adds mappings for electric semicolon to MODE-MAP.
Press ; for electric-semicolon, C-; to insert a semicolon."
  (evil-declare-key 'insert mode-map ";" 'gf/electric-semicolon)
  (evil-declare-key 'insert mode-map (kbd "C-;") (lambda()
                                                   (interactive)
                                                   (insert ";"))))

(defun gf/electric-semicolon ()
  "Inserts a semicolon at the end of the current line if not already there."
  (interactive)
  (let ((beg (point)))
    (end-of-line)
    (if (not (looking-back ";"))
        (insert ";")
      (goto-char beg)
      )))

(defun gf/make-capture-frame ()
  "Make a new frame for using org-capture."
  (interactive)
  (make-frame '((name . "capture") (width . 80) (height . 20)))
  (select-frame-by-name "capture")
  (org-capture))

(defun gf/fix-double-capital()
  "Go back to last occurence of a 'double capital' and correct."
  (interactive)
  ;; <<<<<<< glynn : *old code* >>>>>>>>
  ;;   (save-excursion
  ;;    (re-search-backward "[[:upper:]]\\{2\\}"
  ;;                        nil
  ;;                        (message "No double capital found!"))
  ;;    (forward-char)
  ;;    (set-mark-command nil)
  ;;    (forward-char)
  ;;    (setq deactivate-mark nil)
  ;;    (call-interactively 'downcase-region)))
  ;; =======  *test code* ==============
  (re-search-backward "[[:upper:]]\\{2\\}"
                      nil
                      (message "No double capital found!")))

;; >>>>>>> *end* <<<<<<<<<<<<<<<<<<<<<
(provide 'defuns)
