(require 'auto-complete-config)
;; yasnippet / auto-complete fix
(defun ac-yasnippet-candidates ()
  (with-no-warnings
 (cond (;; 0.8 onwards
           (fboundp 'yas-active-keys)
           (all-completions ac-prefix (yas-active-keys)))
          (;; >0.6.0
           (fboundp 'yas/get-snippet-tables)
           (apply 'append (mapcar 'ac-yasnippet-candidate-1
                                  (condition-case nil
                                      (yas/get-snippet-tables major-mode)
                                    (wrong-number-of-arguments
                                     (yas/get-snippet-tables)))))
           )
          (t
           (let ((table
                  (if (fboundp 'yas/snippet-table)
                      ;; <0.6.0
                      (yas/snippet-table major-mode)
                    ;; 0.6.0
                    (yas/current-snippet-table))))
             (if table
			(ac-yasnippet-candidate-1 table)))))))

(ac-define-source yasnippet-glynn
  '((depends yasnippet)
	(candidates . ac-yasnippet-candidates)
	(candidate-face . ac-yasnippet-candidate-face)
	(selection-face . ac-yasnippet-selection-face)
	(symbol . "snip")))

(defun ac-config-glynn ()
  (setq-default ac-sources '(
							 ac-source-yasnippet-glynn
							 ac-source-dictionary
							 ac-source-words-in-same-mode-buffers
							 ac-source-filename
							 ))
  (add-hook 'emacs-lisp-mode-hook (lambda()
									(setq ac-sources (append '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols) ac-sources))))
  (add-hook 'css-mode-hook (lambda()
							 (setq ac-sources (append '(ac-source-css-property) ac-sources))))
  (global-auto-complete-mode t)
  (setq ac-auto-start 2))

;; To make a new line instead of accepting suggested word, use C-<return>

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-set-key (kbd "C-SPC") 'auto-complete)

(ac-config-glynn)

(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(provide 'setup-autocomplete)
