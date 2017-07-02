(require 'ace-jump-mode)


;; Load ssh credentials from keychain, even if keychain was called
;; after emacs startup
(require 'keychain-environment)
(define-key evil-normal-state-map ",k" (lambda ()
                                         (interactive)
                                         (keychain-refresh-environment)
                                         (message "Keychain environment refreshed.")))

(provide 'setup-general)
