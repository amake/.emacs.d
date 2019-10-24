;;; flycheck-sassc.el --- Sass syntax checker -*- lexical-binding: t -*-

;; Check Sass syntax with dart-sass; see https://sass-lang.com/libsass

;;; Commentary:

;; Check Sass syntax with dart-sass.

;;; Code:

(require 'flycheck)

(flycheck-define-checker dart-sass
  "A Sass syntax checker using dart-sass.

See URL `https://sass-lang.com/dart-sass'."
  :command ("dart-sass" "--stdin" "--no-color")
  :standard-input t
  :error-patterns
  ((error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "╷"
          (minimal-match (zero-or-more
                          (optional "\r") "\n" (zero-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "- " line ":" column (one-or-more not-newline)
          line-end)
   (warning line-start
            "WARNING: "
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more not-newline)))
            (zero-or-more (optional "\r") "\n")
            (optional "\r") "\n" (one-or-more " ") "╷"
            (minimal-match (zero-or-more
                            (optional "\r") "\n" (zero-or-more not-newline)))
            (optional "\r") "\n" (one-or-more " ") "- " line ":" column (one-or-more not-newline)
            line-end))
  :modes scss-mode)

(defun flycheck-dart-sass-setup ()
  "Set up the flycheck-dart-sass checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'dart-sass))

(provide 'flycheck-dart-sass)
;;; flycheck-dart-sass.el ends here
