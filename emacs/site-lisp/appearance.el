;; Cursor configuration.
(setq evil-insert-state-cursor '("#38a2ea" bar))
(setq evil-normal-state-cursor '("#38a2ea" box))
(setq evil-emacs-state-cursor '("#d72626" bar))

;; Other stuff
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Change opacity of the frame.
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))



(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; View hex colours in the following modes
(dolist (mode '(
                css-mode
                scss-mode
                emacs-lisp-mode
                haskell-mode
                lisp-interaction-mode
                lisp-mode
                          ))
  (font-lock-add-keywords mode
                          '((fontify-hex-colors))))

(provide 'appearance)
