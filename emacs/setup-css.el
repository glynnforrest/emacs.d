(require 'css-mode)

(defun cssEvilChangeToPX ()
  "Change text until `px` in css mode."
  (interactive)
  (if (looking-at-p " ")
      (evil-forward-word-begin))
  (let (( beg (point)))
    (evil-find-char 1 (string-to-char "p"))
    (evil-change beg (point))))

(evil-declare-key 'normal css-mode-map "gc" 'cssEvilChangeToPX)

(require 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)


(provide 'setup-css)
