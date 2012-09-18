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
(set-frame-font "DejaVu Sans Mono 8")
(setq default-frame-alist '((font . "DejaVu Sans Mono 8")))


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

;; Setup line numbers and remove fringes
(setq linum-format (lambda (line)
  (propertize
   (format (concat "%"
                   (number-to-string
                    (length (number-to-string
                             (line-number-at-pos (point-max)))))
                   "d ")
           line)
   'face 'linum)))

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
(require 'nyan-mode)
(nyan-mode t)
(nyan-start-animation)

(provide 'init-appearance)
