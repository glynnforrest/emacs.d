(use-package rotate-text
  :commands (gf/clever-rotate-text gf/clever-rotate-text-backward)
  :init
  (defhydra hydra-rotate-text ()
    "Rotate text"
    ("n" gf/clever-rotate-text "Next")
    ("p" gf/clever-rotate-text-backward "Previous")
    ("q" nil "Quit"))

  :config
  (defun gf/clever-rotate-text ()
    "Wrapper to rotate-text that will try the start of the line as well
as the current word."
    (interactive)
    (if (not (condition-case nil
                 (rotate-text 1)
               (error nil)))
        (save-excursion
          (evil-first-non-blank)
          (rotate-text 1))))

  (defun gf/clever-rotate-text-backward ()
    "Wrapper to rotate-text-backward that will try the start of the
line as well as the current word."
    (interactive)
    (if (not (condition-case nil
                 (rotate-text-backward 1)
               (error nil)))
        (save-excursion
          (evil-first-non-blank)
          (rotate-text-backward 1)))))


(provide 'setup-rotate-text)
