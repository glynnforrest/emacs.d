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


(defun gf/split-window-and-move-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun gf/split-window-and-move-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

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
