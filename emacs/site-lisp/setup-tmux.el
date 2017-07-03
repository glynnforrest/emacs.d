(defvar gf/tmux-previous-cmd nil)

(defun gf/tmux-do-run (cmd)
  "Run a command in the previous tmux pane."
  (save-buffer)
  (setq gf/tmux-previous-cmd cmd)
  (shell-command-to-string (concat "tmux last-pane && tmux send-keys -l '" cmd "' && tmux send-keys 'Enter' && tmux last-pane")))

(defun gf/tmux-run (cmd)
  "Run a tmux command in the previous tmux pane."
  (interactive "MCommand: ")
  (gf/tmux-do-run cmd))

(defun gf/tmux-run-last ()
  "Run the last command in the previous tmux pane."
  (interactive)
  (if gf/tmux-previous-cmd
      (gf/tmux-do-run gf/tmux-previous-cmd)
    (call-interactively 'gf/tmux-execute-command)))

(defmacro gf/tmux-def-command (name command key)
  "Create a new function gf/tmux-command-NAME and bind it to SPC c `KEY'."
  (let ((function (intern (format "gf/tmux-command-%s" name))))
    `(progn
       (defun ,function ()
         (interactive)
         (gf/tmux-do-run ,command))

       (general-define-key
        :states '(normal visual insert emacs)
        :prefix "SPC"
        :non-normal-prefix "M-SPC"
        ,(concat "c" key) '(,function :which-key ,name)))))

(general-define-key
 :states '(normal)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "c" '(:ignore t :which-key "tmux commands"))

(provide 'setup-tmux)
