;;; flycheck-shfmt.el --- Lint shell scripts -*- lexical-binding: t -*-

;; Shell script linting using shfmt; see https://github.com/mvdan/sh

;; Package-Requires: ((emacs "25.1") (flycheck "0.25"))

;;; Commentary:

;;; Code:

(require 'flycheck)

(flycheck-define-checker sh-shfmt
  "A shell script syntax checker using shfmt.

See URL `https://github.com/mvdan/sh'."
  :command ("shfmt"
            (eval (flycheck-shfmt-get-parser-opts)))
  :standard-input t
  :error-patterns
  ((error line-start
          ;; filename:line:column: message
          "<standard input>:" line ":" column ":" (zero-or-more " ") (message)
          line-end))
  :modes sh-mode
  :predicate (lambda () (memq sh-shell '(bash sh mksh)))
  :next-checkers ((warning . sh-shellcheck)))

(defun flycheck-shfmt-get-parser-opts ()
  "Get the appropriate parser options for the current buffer."
  (cond ((eq sh-shell 'bash) '("--ln" "bash"))
        ((eq sh-shell 'mksh) '("--ln" "mksh"))
        ((eq sh-shell 'sh) '("--ln" "posix"))))

(defun flycheck-shfmt-setup ()
  "Set up the flycheck-shfmt checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'sh-shfmt))

(provide 'flycheck-shfmt)

;;; flycheck-shfmt.el ends here
