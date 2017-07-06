(defun gf/org-select-project-file-header ()
  "Visit a location to store a new note in the current project"
  (interactive)
  (find-file (gf/org-resolve-project-org-file))
  (goto-char (point-min))
  (let ((choice (completing-read "Project heading: " (gf/org-get-top-level-headings))))
    (re-search-forward (format "^\* %s" choice))))

(defun gf/org-get-top-level-headings ()
  "Get the names of the top level headings in the current org file."
  (save-excursion
    (goto-char (point-min))
    (let ((headings nil))
      (while (re-search-forward "^\\* \\(.+\\)" nil t)
        (add-to-list 'headings (match-string-no-properties 1) t))
      headings)))

(defun gf/org-select-next-task ()
  "Select an item marked as NEXT in the current buffer."
  (interactive)
  (let* ((choices (gf/org--get-keyword-items "NEXT"))
         (choice (cadr (assoc (completing-read "Next task: " choices) choices))))
    (goto-char (point-min))
    (re-search-forward choice)
    (beginning-of-line)
    (gf/org-show-entry)))

(defun gf/org--get-keyword-items (keyword)
  "Get the names of all headings with todo keyword KEYWORD."
  (save-excursion
    (goto-char (point-min))
    (let ((headings nil))
      (while (re-search-forward (format "\\(^\\*+ %s .+\\)" keyword) nil t)
        (let* ((match (match-string-no-properties 1))
               (parent-headings nil)
               (label nil))
          (save-excursion
            (while (not (equal (org-outline-level) 1))
              (outline-up-heading 1 t)
              (beginning-of-line)
              (re-search-forward "^\\*+ \\(.+\\)")
              (add-to-list 'parent-headings (match-string-no-properties 1))))
          (setq label (seq-reduce (lambda (initial item)
                                    (if (equal initial match)
                                        (concat match " | " item)
                                      (concat initial " - " item)))
                                  parent-headings
                                  match))
          (add-to-list 'headings (list label match) t)))
      headings)))

(defun gf/org-show-entry ()
  "Show the current entry and all parent headers."
  (interactive)
  (outline-show-entry)
  (save-excursion
    (outline-up-heading 1 t)
    (outline-show-branches)))

(provide 'defuns-org)
