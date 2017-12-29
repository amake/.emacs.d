;;; amk-macos.el --- macOS-specific functions

;;; Commentary:

;;; Code:

(defun open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (shell-command "open ."))

(defun open-file ()
  "Open the current file in the default app."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "open " (buffer-file-name)))
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

(provide 'amk-macos)
;;; amk-macos.el ends here
