(defun gf/tmux-execute-last-command ()
  "Execute the last tmux command in the previous pane."
  (interactive)
  (save-buffer)
  (shell-command-to-string "tmux last-pane && tmux send-keys 'up' 'Enter' && tmux last-pane"))

(provide 'setup-tmux)
