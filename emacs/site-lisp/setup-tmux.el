(defvar gf/tmux--target-panes nil "p-list of tmux sessions and their target panes.")
(defvar gf/tmux--previous-cmds nil "p-list of tmux sessions and their previous commands.")

(defun gf/tmux--list-sessions ()
  (split-string (shell-command-to-string
                 "tmux list-sessions") "\n"))

(defun gf/tmux--set-target-session (session)
  "Set the name of the tmux session for the current frame."
  (set-frame-parameter nil 'tmux-session session))

(defun gf/tmux--get-target-session ()
  "Get the name of the tmux session for the current frame, prompting for one if not set yet."
  (unless (frame-parameter nil 'tmux-session)
    (gf/tmux--set-target-session
     (car
      (split-string
       (completing-read "Tmux session for this frame: " (gf/tmux--list-sessions) nil t) ":"))))
  (frame-parameter nil 'tmux-session))

(defun gf/tmux--list-panes (session)
  (split-string (shell-command-to-string
                 (format "tmux list-panes -st %s -F \"#{window_index}.#{pane_index} #{pane_current_command} in #{pane_current_path}\" | column -t" session)) "\n"))

(defun gf/tmux--set-target-pane (session pane)
  "Set the target pane to run tmux commands in."
  (setq gf/tmux--target-panes
        (plist-put gf/tmux--target-panes session pane)))

(defun gf/tmux--get-target-pane (session)
  "Get the target pane for the given tmux session, prompting for one if not set yet."
  (when (or (not (plist-member gf/tmux--target-panes session)) current-prefix-arg)
    (gf/tmux--set-target-pane
     session
     (concat
      session ":"
      (car
       (split-string
        (completing-read "Select target tmux pane: " (gf/tmux--list-panes session) nil t) " ")))))
  (plist-get gf/tmux--target-panes session))

(defun gf/tmux-clear-previous-cmds ()
  (interactive)
  (setq gf/tmux--previous-cmds nil))

(defun gf/tmux--create-command (cmd)
  (let ((target (gf/tmux--get-target-pane (gf/tmux--get-target-session))))
    (format "tmux send-keys -t %s -l '%s' && tmux send-keys -t %s Enter" target cmd target)))

(defun gf/tmux--do-run (cmd)
  (save-buffer)
  (shell-command-to-string
   (gf/tmux--create-command cmd))
  ;; should use cl-pushnew onto a stack of commands for each session
  (setq gf/tmux--previous-cmds
        (plist-put gf/tmux--previous-cmds (gf/tmux--get-target-session) cmd)))

(defun gf/tmux-run (cmd)
  "Run a command in the target tmux pane."
  (interactive "MCommand: ")
  (gf/tmux--do-run cmd))

(defun gf/tmux-run-last ()
  "Run the last command in the previous tmux pane."
  (interactive)
  (if (plist-member gf/tmux--previous-cmds (gf/tmux--get-target-session))
      (gf/tmux--do-run (plist-get gf/tmux--previous-cmds (gf/tmux--get-target-session)))
    (call-interactively 'gf/tmux-run)))

(defun gf/tmux-reset ()
  "Reset the target pane and last command."
  (interactive)
  (setq gf/tmux--target-pane nil
        gf/tmux--previous-cmd nil))


(defmacro gf/tmux-def-command (name command &rest args)
  "Create a new function gf/tmux-command-NAME to run COMMAND in a tmux pane.

COMMAND should be a string, or a lisp form that returns a string.
When a lisp form, it will be evaluated when the function is called.


Optional properties:

:key The string name of a key to bind the function to SPC c `KEY'

:project-root If t, run the command inside the projectile project root of the current buffer.


Examples:

(gf/tmux-def-command \"ls\" \"ls\")

(gf/tmux-def-command \"figlet buffer name\"
                     (concat \"figlet \" (buffer-file-name))
                     :key \"f\")
"
  (let ((function (intern (s-dashed-words (format "gf/tmux-command-%s" name))))
        (key (plist-get args :key)))
    `(progn
       (defun ,function ()
         (interactive)
         (gf/tmux--do-run ,(gf/tmux--command-modify command args)))
    ,(when key
       `(general-define-key
        :states '(normal visual insert emacs)
        :prefix "SPC"
        :non-normal-prefix "M-SPC"
        ,(concat "c" key) '(,function :which-key ,name))))))

(defun gf/tmux--command-modify (command args)
  "Modify a command according to the properties in ARGS."
  (when (plist-get args :project-root)
    (setq command `(concat "(cd " (projectile-project-root) " && " ,command ")")))
  command)

(general-define-key
 :states '(normal)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "c" '(:ignore t :which-key "tmux commands"))

(provide 'setup-tmux)
