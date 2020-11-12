;;; flycheck-yard.el --- Lint Ruby YARD docs -*- lexical-binding: t -*-

;; Lint YARD doc comments; see https://yardoc.org/

;; Package-Requires: ((emacs "25.1") (flycheck "0.25"))

;;; Commentary:

;; Lint YARD doc comments.

;;; Code:

(require 'flycheck)

(flycheck-define-checker ruby-yard
  "A Ruby YARD doc checker.

See URL `https://yardoc.org/'."
  :command ("yard" "--no-stats" "--no-progress" "--no-cache"
            "--output-dir" temporary-directory)
  :error-patterns
  ((error line-start "[warn]: In file `" (file-name) "':" line ":" (message) ":" line-end)
   ;; Note that this is the only pattern that uses `file-name`; the others are
   ;; `file-name' (different closing char)
   (error line-start "[warn]: " (message (minimal-match (one-or-more not-newline)))
          " in file `" (file-name) "` near line " line line-end)
   (error line-start "[warn]: " (message) "\n"
          (zero-or-more whitespace) "in file `" (file-name) "' near line " line line-end))
  :working-directory flycheck-ruby--find-project-root
  :modes ruby-mode
  :next-checkers (ruby-rubocop))

(defun flycheck-yard-setup ()
  "Set up the flycheck-yard checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'ruby-yard))

(provide 'flycheck-yard)
;;; flycheck-yard.el ends here
