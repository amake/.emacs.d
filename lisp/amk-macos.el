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

(provide 'amk-macos)
;;; amk-macos.el ends here
