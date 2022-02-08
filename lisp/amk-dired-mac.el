;;; amk-dired-mac.el --- dired utilities for macOS

;; Copyright (C) 2021 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; `dired' utilities for macOS

;;; Code:

(require 'dired)

(defun amk-dired-mac-open-in-finder ()
  "Open the file at point in the Finder."
  (interactive)
  (shell-command
   (format "open %s"
           (shell-quote-argument (dired-get-file-for-visit)))))

(defun amk-dired-mac-reveal-in-finder ()
  "Reveal the file at point in the Finder."
  (interactive)
  (shell-command
   (format "open -R %s"
           (shell-quote-argument (dired-get-file-for-visit)))))

(provide 'amk-dired-mac)
;;; amk-dired-mac.el ends here
