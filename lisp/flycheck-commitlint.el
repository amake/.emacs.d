;;; flycheck-commitlint.el --- Lint git commit messages -*- lexical-binding: t -*-

;; Lint git commit messages; see https://commitlint.js.org/

;; Package-Requires: ((emacs "25.1") (flycheck "0.25") (git-commit "1.0"))

;;; Commentary:

;; Lint git commit messages

;;; Code:

(require 'flycheck)
(require 'git-commit)

(defcustom flycheck-commitlint-config-file nil
  "A custom config file for commitlint."
  :group 'flycheck-commitlint
  :type 'string)

(put 'flycheck-commitlint-config-file 'safe-local-variable #'stringp)

(flycheck-define-checker commitlint
  "A git commit message linter.

See https://commitlint.js.org/"
  :command ("commitlint" "--no-color"
            (eval (when flycheck-commitlint-config-file
                    `("--config" ,flycheck-commitlint-config-file))))
  :standard-input t
  :working-directory flycheck-commitlint--find-root
  :error-patterns
  ((error line-start "✖" (zero-or-more blank) (message) "[" (id (one-or-more (not (in "]")))) "]" line-end))
  :error-filter flycheck-fill-empty-line-numbers
  :modes text-mode
  :enabled flycheck-commitlint--enabled-p)

(defun flycheck-commitlint--find-root ()
  "Find the root of the git repo containing the current buffer's file."
   (vc-find-root (buffer-file-name) ".git"))

(defconst flycheck-commitlint--default-config-files
  '("commitlint.config.js" ".commitlintrc.js" ".commitlintrc" ".commitlintrc.json" ".commitlintrc.yml")
  "Config file names implicitly referenced by commitlint.")

(defun flycheck-commitlint--enabled-p ()
  "Determine whether the checker should be enabled for the current buffer."
  (and
   git-commit-mode
   (or
    flycheck-commitlint-config-file
    (let ((root (flycheck-commitlint--find-root)))
      (seq-find
       (lambda (f) (file-exists-p (concat root f)))
       flycheck-commitlint--default-config-files)))))

(defun flycheck-commitlint-setup ()
  "Set up the flycheck-commitlint checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'commitlint))

(provide 'flycheck-commitlint)
;;; flycheck-commitlint.el ends here
