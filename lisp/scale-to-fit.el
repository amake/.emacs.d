;;; scale-to-fit.el --- Scale text to fit window -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24") face-remap)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Automatically scale text to fit the current window width.

;;; Code:

(require 'face-remap)

(defun scale-to-fit-calculate-scale (width)
  "Calculate the appropriate text scale value to fit WIDTH."
  (let ((curr-width (- (window-body-width)
                       (line-number-display-width))))
    (log (/ curr-width (float width))
         text-scale-mode-step)))

(defun scale-to-fit (width shrink-only)
  "Scale text down if window is narrower than WIDTH.
If SHRINK-ONLY is non-nil, do not enlarge text beyond scale 0."
  (let* ((raw-scale (scale-to-fit-calculate-scale width))
         (scale (if (and shrink-only (> raw-scale 0))
                    0
                  raw-scale)))
    ;; (message "Scaling text: curr-width: %d, target: %d, scale: %f"
    ;;          curr-width width scale)
    (unless (or (= scale text-scale-mode-amount))
      (text-scale-set scale))))

(defmacro scale-to-fit-setup (width shrink-only)
  "Set hooks to call `scale-text-to-fit' when appropriate.
WIDTH and SHRINK-ONLY are per `scale-text-to-fit'."
  `(let ((fun (lambda (&optional _)
                (scale-to-fit ,width ,shrink-only))))
     (add-hook 'hack-local-variables-hook fun nil t)
     (add-hook 'window-size-change-functions fun nil t)))

(provide 'scale-to-fit)
;;; scale-to-fit.el ends here
