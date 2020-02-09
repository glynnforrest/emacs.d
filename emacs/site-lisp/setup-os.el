(defvar gf/is-mac (equal system-type 'darwin))
(defvar gf/is-linux (equal system-type 'gnu/linux))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(provide 'setup-os)
