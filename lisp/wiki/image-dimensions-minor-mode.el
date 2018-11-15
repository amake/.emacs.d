;;; image-dimensions-minor-mode.el --- Display image dimensions in mode line -*- lexical-binding: t -*-
;;
;; Author: Phil S., Aaron Madlon-Kay
;;
;; Compatibility: GNU Emacs 24.3
;;
;; Installation:
;; (eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

;;; Commentary:
;;
;; To also see this information in the frame title, your `frame-title-format'
;; variable might be something like the following:
;;
;; (setq frame-title-format
;;       '(buffer-file-name
;;         ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
;;         (dired-directory
;;          (:eval (concat (buffer-name) " (Emacs) " dired-directory))
;;          ("%b (Emacs)"))))

;;; Code:

(eval-when-compile (require 'cl-macs))

(declare-function image-get-display-property "image-mode")

(defvar-local image-dimensions-minor-mode-dimensions nil
  "Buffer-local image dimensions for `image-dimensions-minor-mode'")

(define-minor-mode image-dimensions-minor-mode
  "Displays the image dimensions in the mode line."
  :init-value nil
  :lighter image-dimensions-minor-mode-dimensions
  (when (not image-dimensions-minor-mode-dimensions)
    (image-dimensions-update-lighter)))

(defun image-dimensions-get-dimensions ()
  "Obtain the size in pixels of the image."
  (let ((image (image-get-display-property)))
      (when image
        (image-size image :pixels))))

(defun image-dimensions-get-size-string ()
  "Generate the size string to be displayed in the lighter."
  (let ((dimensions (image-dimensions-get-dimensions)))
    (when dimensions
      (cl-destructuring-bind (width . height)
          dimensions
        (format " (%dx%d)" width height)))))

(defun image-dimensions-update-lighter (&rest args)
  "Update the lighter string.  Ignore ARGS."
  (let ((dimensions (image-dimensions-get-size-string)))
     (when dimensions
       (setq image-dimensions-minor-mode-dimensions dimensions))))

(add-hook 'image-mode-hook #'image-dimensions-minor-mode)

(provide 'image-dimensions-minor-mode)
;;; image-dimensions-minor-mode.el ends here
