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

;; Start in insert mode / emacs for some modes
(add-to-list 'evil-emacs-state-modes 'package-menu-mode)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'occur-mode 'normal)

(provide 'setup-evil)
