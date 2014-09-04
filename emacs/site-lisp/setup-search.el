(require 'helm-regexp)

;; here are some search / replace workflows

;; open a project file
;; (helm-ls-git-ls) ,f

;; open many project files
;; (helm-ls-git-ls) ,f
;; select candidates with C-<space>
;; select all current candidates with M-a
;; <return>

;; grep in selected project files
;; (helm-ls-git-ls) ,f
;; select candidates with C-<space>
;; select all current candidates with M-a
;; launch grep with C-s
;; search
;; <return>

;; grep in arbitrarily selected files
;; (helm-find-files) ,F
;; select candidates with C-<space>
;; select all current candidates with M-a
;; launch grep with C-s
;; search
;; <return>

;; ag in a project with a view to editing the content
;; (projectile-ag) C-c g
;; search
;; wgrep mode ,e
;; make changes
;; finish edit ,e
;; save buffers ,w

;; opening a file in a non-git project
;; (helm-projectile) ,,f
;; this is still good, but doesn't offer multiple files or grepping

;; bk-helm-occur courtesy of https://news.ycombinator.com/item?id=6873665
(defun get-point-text ()
  "Get 'interesting' text at point; either word, or region"
  (if mark-active
      (buffer-substring (mark) (point))
    (thing-at-point 'symbol)))

(defun helm-occur-1 (initial-value)
  "Preconfigured helm for Occur with initial input."
  (helm-occur-init-source)
  (let ((bufs (list (buffer-name (current-buffer)))))
    (helm-attrset 'moccur-buffers bufs helm-source-occur)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable
     'helm-multi-occur-buffer-tick
     (cl-loop for b in bufs
              collect (buffer-chars-modified-tick (get-buffer b)))))
  (helm :sources 'helm-source-occur
        :buffer "*helm occur*"
        :history 'helm-grep-history
        :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                        (format "%s:%d:" (buffer-name) (line-number-at-pos (point))))
        :truncate-lines t
        :input initial-value))

(defun bk-helm-occur ()
  "Invoke helm-occur with initial input configured from text at point"
  (interactive)
  (helm-occur-1 (get-point-text)))

(define-key evil-normal-state-map ",o" 'helm-occur)
(define-key evil-normal-state-map ",O" 'bk-helm-occur)
(define-key evil-normal-state-map (kbd "C-c o") 'helm-multi-occur)

;; Grep mode
(defun gf/goto-occurrence-recenter ()
  "Go to the occurrence on the current line and recenter."
  (interactive)
  (compile-goto-error)
  (recenter))

(defun gf/display-occurrence-recenter ()
  "Display the occurrence on the current line in another window and recenter."
  (interactive)
  (grep-goto-occurrence-recenter)
  (other-window 1))

(evil-declare-key 'normal ag-mode-map (kbd "<return>") 'gf/display-occurrence-recenter)
(evil-declare-key 'normal ag-mode-map (kbd ",<return>") 'gf/goto-occurrence-recenter)
(evil-declare-key 'normal ag-mode-map (kbd "RET") 'gf/display-occurrence-recenter)
(evil-declare-key 'normal ag-mode-map (kbd ",RET") 'gf/goto-occurrence-recenter)

(define-key evil-normal-state-map (kbd "C-c g") 'projectile-ag)

(require 'wgrep)
(require 'wgrep-ag)

(evil-declare-key 'normal ag-mode-map ",e" 'wgrep-change-to-wgrep-mode)
(evil-declare-key 'normal ag-mode-map ",w" 'wgrep-save-all-buffers)
(evil-declare-key 'normal wgrep-mode-map ",e" 'wgrep-finish-edit)
(evil-declare-key 'normal wgrep-mode-map ",x" 'wgrep-abort-changes)

(provide 'setup-search)
