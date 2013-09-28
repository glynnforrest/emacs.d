(require 'projectile)
(projectile-global-mode)

(setq org-projects-dir "~/notes/projects")

(setq gf/code-projects '(("new" "new")))

(defun gf/new-code-project ()
  "Choose the root directory of a new code project and then open it
with `gf/open-code-project`. The project will be added to
`gf/code-projects`."
  (interactive)
  (let ((project-root (substring (read-directory-name "Project root:") 0 -1)))
    (let ((project-name (car (last (split-string project-root "/")))))
      (if (not (assoc project-name gf/code-projects))
      (setq gf/code-projects
        (append gf/code-projects
            (list (list project-name project-root)))))
      (gf/start-code-project project-name))))

(defun gf/start-code-project (project-name)
  "Open the code project PROJECT-NAME. This will create a new
elscreen, open eshell in the root directory of the project, open the
org-mode project file and a repl if available."
  (if (not (assoc project-name gf/code-projects))
      (error "Project not found!"))
  (if (get-buffer "*eshell*")
      (kill-buffer "*eshell*"))
  (let ((project-root (cadr (assoc project-name gf/code-projects))))
    (gf/switch-to-scratch-buffer)
    (cd project-root)
    (setq gf/current-project-file (gf/create-org-path project-name))
    (setq gf/previous-project-buffer nil)
    (gf/switch-to-project-org-file)
    (split-window-right)
    (eshell)
    ))

(defun gf/create-org-path (path)
  "Create a name suitable for an org file from the last part of a file
path."
  (downcase
   (concat org-projects-dir "/"
           (replace-regexp-in-string
            "\\." "-" (if (equal (substring path 0 1) ".")
                          (substring path 1) path))
           ".org")))

(defun gf/open-code-project ()
  "Open a code project with `gf/start-code-project`, choosing from the
  list `gf/code-projects`."
  (interactive)
  (gf/start-code-project
   (ido-completing-read "Open code project"
            (mapcar 'car gf/code-projects))))

(defun gf/switch-to-project-org-file ()
  "Switch to the org file for the current project."
  (interactive)
  (if (and (boundp 'gf/current-project-file) (stringp gf/current-project-file))
      (elscreen-find-file gf/current-project-file)
    (error "No project open.")))

(defun gf/toggle-switch-to-project-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (if (not (and (boundp 'gf/previous-project-buffer) (stringp gf/previous-project-buffer)))
      ;; if prev is not defined and we're in the org file, error out
      (if (eq (buffer-file-name) gf/current-project-file)
          (error "No previous file in this project.")
        ;;if prev is defined and we're not in the org file, go to it
        ;;and set current buffer as prev
        (progn
          (setq gf/previous-project-buffer (buffer-name))
          (gf/switch-to-project-org-file)
          ))
    ;; if the current buffer is the org file, don't set prev and go to
    ;; the prev buffer
    (if (equal (buffer-file-name) (file-truename gf/current-project-file))
        (progn
          (elscreen-find-and-goto-by-buffer gf/previous-project-buffer))
        ;; if the current buffer isn't the org file, set prev and go
        ;; to org file
      (progn
        (setq gf/previous-project-buffer (buffer-name))
        (gf/switch-to-project-org-file)
        ))))

(define-key global-map (kbd "C-c C-n") 'gf/new-code-project)
(define-key global-map (kbd "C-c C-o") 'gf/open-code-project)

(provide 'setup-projects)
