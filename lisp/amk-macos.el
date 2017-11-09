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

(provide 'amk-macos)
;;; amk-macos.el ends here
