;;; amk-dired.el --- dired utilitiesOS

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
    (if files
        (apply #'ediff-files files)
      (error "No files marked"))))

(provide 'amk-dired)
;;; amk-dired.el ends here
