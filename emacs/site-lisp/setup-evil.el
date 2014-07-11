;; < Evil is the only way to use Emacs!!! >
;;  ------------------------------------
;;       \                    / \  //\
;;        \    |\___/|      /   \//  \\
;;             /0  0  \__  /    //  | \ \
;;            /     /  \/_/    //   |  \  \
;;            @_^_@'/   \/_   //    |   \   \
;;            //_^_/     \/_ //     |    \    \
;;         ( //) |        \///      |     \     \
;;       ( / /) _|_ /   )  //       |      \     _\
;;     ( // /) '/,_ _ _/  ( ; -.    |    _ _\.-~        .-~~~^-.
;;   (( / / )) ,-{        _      `-.|.-~-.           .~         `.
;;  (( // / ))  '/\      /                 ~-. _ .-~      .-~^-.  \
;;  (( /// ))      `.   {            }                   /      \  \
;;   (( / ))     .----~-.\        \-'                 .~         \  `. \^-.
;;              ///.----..>        \             _ -~             `.  ^-`  ^-_
;;                ///-._ _ _ _ _ _ _}^ - - - - ~                     ~-- ,.-~
;;                                                                   /.-~
(require 'evil)
(evil-mode 1)

(setq evil-default-cursor t)

(require 'evil-numbers)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)

;; To not miss surround mode
(require 'surround)
(global-surround-mode t)
(global-auto-revert-mode t)

;; Centre screen around a search
(defadvice
    evil-search-forward
    (after evil-search-forward-recenter activate)
    (recenter))
(ad-activate 'evil-search-forward)

(defadvice
    evil-search-next
    (after evil-search-next-recenter activate)
    (recenter))
(ad-activate 'evil-search-next)

(defadvice
    evil-search-previous
    (after evil-search-previous-recenter activate)
    (recenter))
(ad-activate 'evil-search-previous)

;; Create lines above and below in normal and insert mode with <return>
(define-key evil-normal-state-map (kbd "S-<return>") (lambda()
													   (interactive)
													   (evil-open-below 1)
													   (evil-normal-state 1)))
(define-key evil-normal-state-map (kbd "C-S-<return>") (lambda()
														 (interactive)
														 (evil-open-above 1)
														 (evil-normal-state 1)))
(define-key evil-insert-state-map (kbd "S-<return>") (lambda()
													   (interactive)
													   (evil-open-below 1)))
(define-key evil-insert-state-map (kbd "C-S-<return>") (lambda()
														 (interactive)
														 (evil-open-above 1)))

;; Start in insert mode / emacs for some modes
(add-to-list 'evil-emacs-state-modes 'package-menu-mode)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'occur-mode 'normal)

;; Expand region
(require 'expand-region)
(define-key evil-normal-state-map ",v" 'er/expand-region)

(eval-after-load "evil" '(setq expand-region-contract-fast-key "V"
                               expand-region-reset-fast-key "r"))

(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

(defun gf/evil-forward-arg (count)
  "Small wrapper around evil-forward-arg when at the opening bracket."
  (interactive "p")
  (if (looking-at-p "(")
      (forward-char))
  (evil-forward-arg count)
  )

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'gf/evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

(provide 'setup-evil)
