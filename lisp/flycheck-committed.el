;;; flycheck-committed.el --- Lint git commit messages -*- lexical-binding: t -*-

;; Lint git commit messages; see https://github.com/crate-ci/committed

;; Package-Requires: ((emacs "25.1") (flycheck "0.25") (git-commit "1.0"))

;;; Commentary:

;; Lint git commit messages

;;; Code:

(require 'flycheck)
(require 'git-commit)
(require 'vc-hooks)

(defcustom flycheck-committed-config-file nil
  "A custom config file for committed."
  :group 'flycheck-committed
  :type 'string)

(put 'flycheck-committed-config-file 'safe-local-variable #'stringp)

(flycheck-define-checker committed
  "A git commit message linter.

See https://github.com/crate-ci/committed"
  :command ("committed" "--commit-file" "-"
            (eval (when flycheck-committed-config-file
                    `("--config" ,flycheck-committed-config-file))))
  :standard-input t
  :working-directory (lambda (_) (vc-find-root (buffer-file-name) ".git"))
  :error-patterns
  ((error line-start "-: error " (message) line-end))
  :error-filter flycheck-fill-empty-line-numbers
  :modes text-mode
  :enabled flycheck-committed--enabled-p)

(defun flycheck-committed--enabled-p ()
  "Determine whether the checker should be enabled for the current buffer."
  git-commit-mode)

(defun flycheck-committed-setup ()
  "Set up the flycheck-committed checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'committed))

(provide 'flycheck-committed)
;;; flycheck-committed.el ends here
