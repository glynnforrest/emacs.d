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
(set-frame-font "DejaVu Sans Mono 9")
(setq default-frame-alist '((font . "DejaVu Sans Mono 9")))


;; Load a theme
(require 'color-theme-monokai)
(color-theme-monokai)

;; Cursor configuration.
(blink-cursor-mode -1)
(setq evil-insert-state-cursor '("#38a2ea" bar))
(setq evil-normal-state-cursor '("#38a2ea" box))
(setq evil-emacs-state-cursor '("#d72626" bar))

;; Pretty parenthesis
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;; Fringes are a pain
(fringe-mode -1)

;; Change opacity of the frame.
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
	decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
		 (oldalpha (if alpha-or-nil alpha-or-nil 100))
		 (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
	(when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
	  (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; Write room
(defvar writeroom-enabled nil)
(autoload 'hide-mode-line "hide-mode-line" nil t)
(defun toggle-writeroom ()
  (interactive)
  (if (not writeroom-enabled)
	  (setq writeroom-enabled t)
	(setq writeroom-enabled nil))
	(hide-mode-line)
	(global-linum-mode -1)
	(if writeroom-enabled
		(progn
		  (fringe-mode 'both)
		  ;; (menu-bar-mode -1)
		  (set-fringe-mode 200))
	  (progn
		(set-fringe-mode 0)
		(fringe-mode -1)
		;; (menu-bar-mode)
		(global-linum-mode 1))))

;; Fun
;; (require 'nyan-mode)
;; (nyan-mode t)
;; (nyan-start-animation)

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

;; Make autocomplete look nice

(custom-set-faces
 '(ac-completion-face ((t (:foreground "darkgray" :underline t))))
 '(ac-candidate-face ((t (:background "gray60" :foreground "black"))))
 '(ac-selection-face ((t (:background "deep pink" :foreground "black"))))
 '(ac-yasnippet-candidate-face ((t (:background "gray60" :foreground "black"))))
 '(ac-yasnippet-selection-face ((t (:background "deep pink" :foreground "black"))))
 '(popup-isearch-match ((t (:background "black" :foreground "deep pink"))))
 '(popup-tip-face ((t (:background "#333333" :foreground "white"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#0A0A0A"))))
 '(popup-scroll-bar-background-face ((t (:background "#333333"))))
 '(show-paren-match-face ((t (:background "deep pink" :foreground "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7cfc00"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#eefd00"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#268bd2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#dc322f"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffffff"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#268bd2"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#859900"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#dc322f"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#b58900"))))
 )

(provide 'appearance)
