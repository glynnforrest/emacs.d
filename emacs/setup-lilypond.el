;; lilypond

(defvar ac-lilypond-identifiers
  '((candidates
     . (lambda ()
	 (all-completions ac-target LilyPond-identifiers)))))

;; I need to do a mapcar on the list of keywords to put an extra \\ in front of the \\ that is there already.
;; This will probably done if any LaTeX is used for auto-complete.

(defvar ni-LilyPond-keywords
  (mapcar (lambda (x) (concat "\\" x)) LilyPond-keywords))

(defvar ac-lilypond-keywords
  '((candidates
     . (lambda ()
	 (all-completions ac-target ni-LilyPond-keywords)))))

(defvar ac-lilypond-Creserved-words
 '((candidates
     . (lambda ()
	 (all-completions ac-target LilyPond-Capitalized-Reserved-Words)))))

 (defvar ac-lilypond-ncreserved-words
   '((candidates
      . (lambda ()
	  (all-completions ac-target LilyPond-non-capitalized-reserved-words)))))

(make-local-variable 'ac-sources)
(setq ac-sources '(ac-lilypond-ncreserved-words
			       ac-lilypond-Creserved-words
			       ac-lilypond-keywords
			       ac-lilypond-identifiers ;these don't work. why?
			       ac-source-abbrev
			       ac-source-words-in-buffer))

(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
(auto-complete-mode t)

(provide 'init-lilypond)
