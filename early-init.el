;;; early-init.el --- My emacs setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'package)

(setq package-user-dir (format "%s-%d" package-user-dir emacs-major-version))

(provide 'early-init)
;;; early-init.el ends here
