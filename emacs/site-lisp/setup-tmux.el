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

(provide 'setup-tmux)
