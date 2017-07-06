(defvar gf/tmux--target-pane nil)
(defvar gf/tmux--previous-cmd nil)

(defun gf/tmux--list-panes ()
  (split-string (shell-command-to-string
                 "tmux list-panes -a -F \"#{session_name}:#{window_index}.#{pane_index} #{pane_current_command} in #{pane_current_path}\" | column -t") "\n"))

(defun gf/tmux--set-target-pane ()
  "Set the target pane to run tmux commands in."
  (setq gf/tmux--target-pane
        (car (split-string
              (completing-read "Select target tmux pane: " (gf/tmux--list-panes) nil t)
              " "))))

(defun gf/tmux--get-target-pane ()
  "Get the target pane to run tmux commands in."
  (when (or (not gf/tmux--target-pane) current-prefix-arg)
    (gf/tmux--set-target-pane))
  gf/tmux--target-pane)

(defun gf/tmux--create-command (cmd)
  (let ((target (gf/tmux--get-target-pane)))
    (format "tmux send-keys -t %s -l '%s' && tmux send-keys -t %s Enter" target cmd target)))

(defun gf/tmux--do-run (cmd)
  (save-buffer)
  (setq gf/tmux--previous-cmd cmd)
  (shell-command-to-string
   (gf/tmux--create-command cmd)))


(defun gf/tmux-run (cmd)
  "Run a command in the target tmux pane."
  (interactive "MCommand: ")
  (gf/tmux--do-run cmd))

(defun gf/tmux-run-last ()
  "Run the last command in the previous tmux pane."
  (interactive)
  (if gf/tmux--previous-cmd
      (gf/tmux--do-run gf/tmux--previous-cmd)
    (call-interactively 'gf/tmux-run)))

(defun gf/tmux-reset ()
  "Reset the target pane and last command."
  (interactive)
  (setq gf/tmux--target-pane nil
        gf/tmux--previous-cmd nil))


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
