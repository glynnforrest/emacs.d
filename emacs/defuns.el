(defun gf-convert-to-end-of-sentence ()
  "Change the current character to a full stop and capitalise the next word."
  (interactive)
  (delete-char 1)
  (insert ".")
  (evil-forward-word-begin)
  (evil-invert-char (point) (+ 1 (point))))

(provide 'defuns)
