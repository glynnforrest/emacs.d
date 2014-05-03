(defun setup-gui ()
  "setup gui elements"
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(setup-gui)

;; Make sure gui stuff is right for new frames too
(add-hook 'after-make-frame-functions (lambda(frame)
                                        (setup-gui)
                                        ))

;; Fonts that work for reloading init.el and new emacsclient instances
(set-frame-font "Ubuntu Mono 11")
(setq default-frame-alist '((font . "Ubuntu Mono 11")))


;; Load a theme
(require 'sanityinc-tomorrow-eighties-theme)
(color-theme-sanityinc-tomorrow-eighties)

;; Cursor configuration.
(blink-cursor-mode -1)
(setq evil-insert-state-cursor '("#38a2ea" bar))
(setq evil-normal-state-cursor '("#38a2ea" box))
(setq evil-emacs-state-cursor '("#d72626" bar))

;; Pretty parenthesis
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;; Fringes are a pain
(when (fboundp 'fringe-mode)
  (fringe-mode -1))

;; Other stuff
(setq line-spacing 1)
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

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

(defun fontify-hex-colors (limit)
  (remove-overlays (point) limit 'fontify-hex-colors t)
  (while (re-search-forward "\\(#[[:xdigit:]]\\{6\\}\\)" limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0))))
      (overlay-put ov 'face  (list :background (match-string 1) :foreground "black"))
      (overlay-put ov 'fontify-hex-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this
  ;; function
  nil)

;; View hex colours in the following modes
(dolist (mode '(
                css-mode
                emacs-lisp-mode
                haskell-mode
                lisp-interaction-mode
                lisp-mode
                          ))
  (font-lock-add-keywords mode
                          '((fontify-hex-colors))))

(provide 'appearance)
