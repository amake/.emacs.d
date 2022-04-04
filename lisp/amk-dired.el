;;; amk-dired.el --- Dired utilities

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; `dired' utilities

;;; Code:

(require 'dired)
(require 'ediff)

(defun amk-dired-ediff ()
  "Diff marked files in ediff."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (cond
     ((cadddr files)
      (error "Must have 2 or 3 files marked"))
     ((caddr files)
      (apply #'ediff-files3 files))
     ((cadr files)
      (apply #'ediff-files files))
     (t (error "Must have 2 or 3 files marked")))))

(provide 'amk-dired)
;;; amk-dired.el ends here
