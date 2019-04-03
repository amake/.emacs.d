;;; scale-to-fit.el --- Scale text to fit window -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.3") face-remap)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Automatically scale text to fit the current window width.  This is useful for
;; e.g. source code buffers that are autoformatted with a particular maximum
;; line length:
;;
;;   (defun foolang-scale-to-fit-setup ()
;;     (scale-to-fit-setup 80))
;;   (add-hook 'foolang-mode-hook #'foolang-scale-to-fit-setup)
;;
;; Minimum and maximum scales can be set globally with `scale-to-fit-min-scale'
;; and `scale-to-fit-max-scale', or locally as arguments to
;; `scale-to-fit-setup'.  By default the maximum scale is 0, i.e. zooming beyond
;; the default size is disabled.

;;; Code:

(require 'face-remap)

(defconst scale-to-fit--debug nil)

(defcustom scale-to-fit-min-scale -2 "Minimum scale for fitting."
  :type 'number
  :safe #'numberp
  :group 'scale-to-fit)

(defcustom scale-to-fit-max-scale 0 "Maximum scale for fitting."
  :type 'number
  :safe #'numberp
  :group 'scale-to-fit)

(defun scale-to-fit--current-width ()
  "Calculate the effective width of the screen in columns."
  (- (window-body-width)
     (line-number-display-width)))

(defun scale-to-fit--calculate-scale (width)
  "Calculate the appropriate text scale value to fit WIDTH columns."
  (log (/ (scale-to-fit--current-width) (float width))
       text-scale-mode-step))

(defun scale-to-fit (width)
  "Scale text down if window is narrower than WIDTH columns.
If SHRINK-ONLY is non-nil, do not enlarge text beyond scale 0."
  (let* ((raw-scale (scale-to-fit--calculate-scale width))
         (scale (min (max raw-scale scale-to-fit-min-scale) scale-to-fit-max-scale)))
    (when scale-to-fit--debug
      (message "Scaling text: curr-width: %d, target: %d, raw scale: %f, adjusted scale: %f"
               (scale-to-fit--current-width) width raw-scale scale))
    (unless (or (= scale text-scale-mode-amount))
      (text-scale-set scale))))

(defmacro scale-to-fit-setup (width &optional min max)
  "Set hooks to call `scale-text-to-fit' when appropriate.

WIDTH should evaluate to the target width in columns.
MIN and MAX are buffer-local values for `scale-to-fit-min-scale' and `scale-to-fit-max-scale'."
  `(let ((fun (lambda (&optional _)
                (scale-to-fit ,width))))
     (when ,min
       (setq-local scale-to-fit-min-scale ,min))
     (when ,max
       (setq-local scale-to-fit-max-scale ,max))
     (add-hook 'hack-local-variables-hook fun nil t)
     (add-hook 'window-size-change-functions fun nil t)))

(provide 'scale-to-fit)
;;; scale-to-fit.el ends here
