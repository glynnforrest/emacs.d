;; ERC
(require 'erc)
(require 'erc-spelling)

;; my-erc-nick should be in personal.el
(setq erc-nick my-erc-nick)

(defun gf-erc-goto-and-clear-prompt ()
  "Go to the bottom of the ERC buffer and clear the input,
placing cursor at the start of the prompt."
  (interactive)
  (goto-line (buffer-end 1))
  (beginning-of-line)
  (evil-forward-WORD-begin)
  (delete-region (point) (line-end-position))
  (evil-insert-state))

;; crappy hack to write and send text to a channel
(defun gf-erc-write-and-send (text)
  "Write `text` in the ERC buffer and send."
  (gf-erc-goto-and-clear-prompt)
  (insert text)
  (erc-send-current-line))

(add-hook 'erc-mode-hook (lambda ()
						   (interactive)
						   (erc-spelling-mode)))

;; print the topic when joining a channel
(add-hook 'erc-join-hook (lambda()
						   (gf-erc-write-and-send "/topic")))

(evil-declare-key 'normal erc-mode-map "C" 'gf-erc-goto-and-clear-prompt)
