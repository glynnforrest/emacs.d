(require 'org)
(require 's)

(defun gf/org-select-project-file-header ()
  "Visit a location to store a new note in the current project."
  (interactive)
  (gf/projects-open-org-file)
  (gf/org-select-top-level-header))

(defun gf/org-select-other-project-file-header ()
  "Visit a location to store a new note in another project."
  (interactive)
  (let ((project (file-truename (completing-read "Project: " (projectile-relevant-known-projects)))))
    (message project)
    (find-file (gf/projects--resolve-org-path project))
    (gf/org-select-top-level-header)))

(defun gf/org-select-top-level-header ()
  "Visit a top level heading in the current file."
  (interactive)
  (goto-char (point-min))
  (let ((choice (completing-read "Heading: " (gf/org--get-top-level-headings))))
    (goto-char (point-min))
    (re-search-forward (format "^\* %s" choice)))
  (outline-show-children))

(defun gf/org-select-top-level-header-or-all (arg)
  "Visit a heading in the current file. Choose from top level headings, or all headings if called with a prefix argument."
  (interactive "P")
  (if arg
      (helm-org-in-buffer-headings)
    (gf/org-select-top-level-header)))

(defun gf/org--get-top-level-headings ()
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

(defun gf/org-insert-blank-title ()
  (interactive)
  (goto-char (point-min))
  (if (looking-at-p ":PROPERTIES:")
      (progn
        (re-search-forward ":END:")
        (forward-line 1)
        (beginning-of-line)))
  (insert "#+TITLE: ")
  (newline)
  (newline)
  (forward-line -2)
  (end-of-line)
  (evil-insert-state))

(defun gf/org-show-entry ()
  "Show the current entry and all parent headers."
  (interactive)
  (outline-show-entry)
  (save-excursion
    (outline-up-heading 1 t)
    (outline-show-branches)))

(defun gf/org-find-file ()
  (interactive)
  (projectile-find-file-in-directory org-directory))

(defun gf/org-up-to-level (level)
  "Move up to the previous LEVEL heading."
  (interactive)
  (while (progn
           (org-up-element)
           (not (looking-at-p (concat (s-repeat level "\\*") " "))))))

(provide 'defuns-org)
