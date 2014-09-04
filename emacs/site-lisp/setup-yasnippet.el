(require 'yasnippet)
;; Don't use bundled snippets
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; cludgy hack to favor candidates over the input by setting REQUIRE-MATCH.
(defun gf/yas-completing-prompt (prompt choices &optional display-fn completion-fn)
  (let* ((formatted-choices
          (if display-fn (mapcar display-fn choices) choices))
         (chosen (funcall (or completion-fn #'completing-read)
                          prompt formatted-choices
                          nil t nil nil)))
    (if (eq choices formatted-choices)
        chosen
      (nth (or (position chosen formatted-choices :test #'string=) 0)
           choices))))

(setq yas-prompt-functions '(gf/yas-completing-prompt))

(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;; don't expand part of words
(setq yas/key-syntaxes '("w_" "w_." "^ "))

;; used by some of the snippets
(defun buffer-file-name-body ()
  (if (buffer-file-name)
	  (first (split-string (file-name-nondirectory (buffer-file-name)) "\\.")))
  )

(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)

(provide 'setup-yasnippet)
