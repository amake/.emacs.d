;;; amk-macos.el --- macOS-specific functions

;;; Commentary:

;;; Code:

(require 'subr-x)

(defun open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (let ((cmd (if buffer-file-name
                 (format "open -R '%s'" buffer-file-name)
               "open .")))
    (shell-command cmd)))

(defun open-file ()
  "Open the current file in the default app."
  (interactive)
  (if (buffer-file-name)
      (shell-command (format "open '%s'" buffer-file-name))
    (error "Buffer is not associated with a file")))

(defun reveal-file ()
  "Reveal the current file in the Finder."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "open -R " (buffer-file-name)))
    (error "Buffer is not associated with a file")))

(defun open-terminal ()
  "Open a macOS Terminal window in the pwd."
  (interactive)
  (shell-command "open -a Terminal ."))

(defun touch ()
  "Touch current buffer's file."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (shell-command (concat "touch " file))
    (shell-command (concat "date -r " file))))

(defvar amk-mac-appearance-mode nil)

(defun set-mac-appearance-mode (mode)
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

(defun toggle-appearance-mode ()
  "Toggle between light mode, dark mode, and automatic."
  (interactive)
  (let* ((mode amk-mac-appearance-mode)
         (next (cond ((eq mode 'light) 'dark)
                     ((eq mode 'dark) nil)
                     (t 'light))))
    (set-mac-appearance-mode next)
    (message "%s" (or next "auto"))))

(provide 'amk-macos)
;;; amk-macos.el ends here
