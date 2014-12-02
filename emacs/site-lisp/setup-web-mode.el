(require 'web-mode)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)

;; Use multiple-cursors to rename tags
(define-key web-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)

;; Sort of like matchit
(evil-declare-key 'normal web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'visual web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'normal web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)
(evil-declare-key 'visual web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)

(require 'scss-mode)
(setq scss-compile-at-save nil)

(require 'sgml-mode)

(require 'emmet-mode)

(defun setup-emmet (map hook)
  "Setup emmet key bindings for MAP and HOOK."
  (add-hook hook 'emmet-mode)
  (define-key map (kbd "C-c .") 'emmet-next-edit-point)
  (define-key map (kbd "C-c ,") 'emmet-prev-edit-point)
  (define-key map (kbd "C-c j") 'emmet-expand-line))

(setup-emmet html-mode-map 'html-mode-hook)
(setup-emmet web-mode-map 'web-mode-hook)
(setup-emmet css-mode-map 'css-mode-hook)

(define-key emmet-mode-keymap (kbd "C-j") nil)
(setq emmet-move-cursor-between-quotes t)
(setq emmet-move-cursor-after-expanding t)

(provide 'setup-web-mode)
