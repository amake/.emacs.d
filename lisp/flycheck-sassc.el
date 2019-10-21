;;; flycheck-sassc.el --- Sass syntax checker -*- lexical-binding: t -*-

;; Check Sass syntax with sassc; see https://sass-lang.com/libsass

;;; Commentary:

;; Check Sass syntax with sassc.

;;; Code:

(require 'flycheck)

(flycheck-define-checker sassc
  "A Sass syntax checker using the SassC compiler.

See URL `https://sass-lang.com/libsass'."
  :command ("sassc"
            "--line-numbers"
            "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "on line " line ":" column
          " of stdin"
          line-end))
  :modes scss-mode)

(defun flycheck-sassc-setup ()
  "Set up the flycheck-sassc checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'sassc))

(provide 'flycheck-sassc)
;;; flycheck-sassc.el ends here
