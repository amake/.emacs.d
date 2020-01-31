;;; emoji-github.el --- Prettify GitHub emoji shortcodes -*- lexical-binding: t -*-

;; Prettify GitHub emoji shortcodes like :dancing_women:

;;; Commentary:

;;; Code:

(defvar emoji-github-alist
  '(;; Would be nicer as "👯‍♀️" but result looks poor
    (":dancing_women:" .  ?👯)))

(defun emoji-github-enable ()
  "Enable GitHub emoji shortcodes for command `prettify-symbols-mode'."
  (setq-local prettify-symbols-alist (append emoji-github-alist prettify-symbols-alist)))

(provide 'emoji-github)
;;; emoji-github.el ends here
