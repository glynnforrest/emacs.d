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

(use-package evil
  :init
  (setq-default
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t)

  :config
  (evil-mode 1)
  (use-package evil-numbers)

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

  (defun gf/visual-shift-left ()
    "Shift left and keep the region active."
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun gf/visual-shift-right ()
    "Shift right and keep the region active."
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

  (add-to-list 'evil-emacs-state-modes 'package-menu-mode)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'org-capture-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'occur-mode 'normal))

;; Save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t))

;; To not miss surround mode
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package undo-tree
  :diminish "")

(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t
              evil-lisp-state-enter-lisp-state-on-command nil))

(use-package expand-region
  :commands er/expand-region
  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "0"))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; exchange two regions or motions with gx. gX cancels a pending swap
(use-package evil-exchange
  :config
  (evil-exchange-install))

(defun gf/evil-forward-arg (count)
  "Small wrapper around evil-forward-arg when at the opening bracket."
  (interactive "p")
  (if (looking-at-p "(")
      (forward-char))
  (evil-forward-arg count)
  )

;; bind evil-forward/backward-args
;; (define-key evil-normal-state-map "L" 'gf/evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

(provide 'setup-evil)
