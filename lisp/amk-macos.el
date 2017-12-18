;;; package --- Summary:
;;; Custom commands with macOS-specific functionality

;;; Commentary:

;;; Code:

(defun open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (shell-command "open ."))

(defun open-file ()
  "Open the current file in the default app."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(defun reveal-file ()
  "Reveal the current file in the Finder."
  (interactive)
  (shell-command (concat "open -R " (buffer-file-name))))

(defun open-terminal ()
  "Open a macOS Terminal window in the pwd."
  (interactive)
  (shell-command "open -a Terminal ."))

(provide 'amk-macos)
;;; amk-macos.el ends here
