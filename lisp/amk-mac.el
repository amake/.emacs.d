;;; amk-mac.el --- Functions specific to macOS

;; Copyright (C) 2017-2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Functions specific to macOS

;;; Code:

(require 'subr-x)

(defun amk-mac-open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (let ((cmd (if buffer-file-name
                 (format "open -R '%s'" buffer-file-name)
               "open .")))
    (shell-command cmd)))

(defun amk-mac-open-file ()
  "Open the current file in the default app."
  (interactive)
  (if (buffer-file-name)
      (shell-command (format "open '%s'" buffer-file-name))
    (error "Buffer is not associated with a file")))

(defun amk-mac-reveal-file ()
  "Reveal the current file in the Finder."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "open -R " (buffer-file-name)))
    (error "Buffer is not associated with a file")))

(defun amk-mac-open-terminal ()
  "Open a macOS Terminal window in the pwd."
  (interactive)
  (shell-command "open -a Terminal ."))

(defun amk-mac-touch ()
  "Touch current buffer's file."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (shell-command (concat "touch " file))
    (shell-command (concat "date -r " file))))

(defvar amk-mac-appearance-mode nil
  "Mac appearance mode: 'light, 'dark, or nil (auto).")

(defun amk-mac-set-appearance-mode (mode)
  "Set Mac appearance MODE (light, dark, auto) independently of system settings."
  (setq amk-mac-appearance-mode mode)
  (cond ((eq mode 'light)
         (setq frame-background-mode 'light)
         (set-background-color "white")
         (set-foreground-color "black"))
        ((eq mode 'dark)
         (setq frame-background-mode 'dark)
         (set-background-color "black")
         (set-foreground-color "white"))
        (t
         (setq frame-background-mode nil)
         (set-background-color "mac:textBackgroundColor")
         (set-foreground-color "mac:textColor")))
  (mapc #'frame-set-background-mode (frame-list)))

(defun amk-mac-toggle-appearance-mode ()
  "Toggle between light mode, dark mode, and automatic."
  (interactive)
  (let* ((mode amk-mac-appearance-mode)
         (next (cond ((eq mode 'light) 'dark)
                     ((eq mode 'dark) nil)
                     (t 'light))))
    (amk-mac-set-appearance-mode next)
    (message "%s" (or next "auto"))))

(provide 'amk-mac)
;;; amk-mac.el ends here
