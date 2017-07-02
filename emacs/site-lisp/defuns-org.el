(defun gf/org-select-project-file-header ()
  "Visit a location to store a new note in the current project"
  (interactive)
  (find-file (gf/org-resolve-project-org-file))
  (goto-char (point-min))
  (let ((choice (completing-read "Select project heading: " (gf/org-get-top-level-headings))))
    (re-search-forward (format "^\* %s" choice))))

(defun gf/org-get-top-level-headings ()
  "Get the names of the top level headings in the current org file."
  (save-excursion
    (goto-char (point-min))
    (let ((headings nil))
      (while (re-search-forward "^\\* \\(.+\\)" nil t)
        (add-to-list 'headings (match-string-no-properties 1) t))
      headings)))


(provide 'defuns-org)
