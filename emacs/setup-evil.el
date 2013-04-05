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
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'occur-mode 'normal)

(provide 'setup-evil)
