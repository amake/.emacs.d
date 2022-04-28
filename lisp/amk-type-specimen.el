;;; amk-type-specimen.el --- Generate type specimens -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Generate type specimens

;;; Code:


(defconst amk-type-specimen-sizes-pts
  '(288 144 96 72 64 48 36 24 18 14 13 12 11 10 9))

(defun amk-type-specimen-create (&optional font)
  "Generate a type specimen with FONT."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (with-output-to-temp-buffer (format "*Specimen: %s*" font)
    (with-current-buffer standard-output
      (setq-local truncate-lines t)
      (if (fboundp #'global-display-line-numbers-mode)
          (global-display-line-numbers-mode -1) ; emacs 26 and later
        (global-linum-mode -1))
      (insert (format "%s\n\n" font))
      (dolist (pts amk-type-specimen-sizes-pts)
        (insert (propertize
                 "The quick brown fox jumped over the lazy dog.\n"
                 'face `(:family ,font :height ,(* pts 10))))
        (insert "\n")))))

(provide 'amk-type-specimen)
;;; amk-type-specimen.el ends here
