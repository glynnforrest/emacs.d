(defun convert-to-end-of-sentence ()
  "Change the current character to a full stop and capitalise the next word."
  (interactive)
  (delete-char 1)
  (insert ".")
  (evil-forward-word-begin)
  (evil-invert-char (point) (+ 1 (point))))

(defun open-url-from-buffer ()
  "Open a url with ido, choosing from all of the urls in the current
buffer."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let ((urls ()))
	  (while (re-search-forward "http:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
		(let ((url (match-string-no-properties 0)))
		  (add-to-list 'urls url)
		  ))
	  (let ((url (ido-completing-read "Open url in buffer: " urls nil t)))
		(when url
		  (browse-url url))))))

(defun open-recent-url ()
  "Open the url closest behind the current point, for example in an
ERC buffer."
  (interactive)
  (save-excursion
	(re-search-backward "http:\/\/[a-z0-9\.\/-_\?=%&]+" nil t)
	(let ((url (match-string-no-properties 0)))
		(when url
		  (browse-url url)))))

(defun evil-org-beginning-of-line ()
  "Move to the beginning of the line in an org-mode file, ignoring
TODO keywords, stars and list indicators."
 (interactive)
 (beginning-of-line)
 (if (looking-at-p " ") (evil-forward-word-begin))
 (if (looking-at-p "*") (evil-forward-word-begin))
 (if (looking-at-p "TODO\\|DONE\\|WAITING") (evil-forward-word-begin)))

(provide 'defuns)
