;;; shfmt.el --- Autoformat shell scripts -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defcustom shfmt-executable "shfmt" "The executable to run when autoformatting."
  :group 'shfmt)

(defcustom shfmt-arguments "" "The args to supply to `shfmt-executable' when autoformatting."
  :group 'shfmt)

(defun shfmt-build-command ()
  "Build the command to execute when autoformatting."
  (mapconcat #'identity `(,shfmt-executable ,shfmt-arguments) " "))

(defun shfmt-enable-on-save ()
  "Pre-save hook for running shfmt."
  (interactive)
  (add-hook 'before-save-hook 'shfmt-buffer nil t))

(defun shfmt-region (start end)
  "Autoformat the region defined by START and END."
  (interactive)
  (if (executable-find shfmt-executable)
      (let* ((prev-point (point))
             (prev-window-start (window-start))
             (error-buffer-name "*Shfmt Errors*")
             (error-buffer (get-buffer-create error-buffer-name))
             (coding-system-for-write buffer-file-coding-system)
             (coding-system-for-read buffer-file-coding-system))
        (if (zerop (shell-command-on-region
                    start end
                    (shfmt-build-command)
                    (current-buffer) t
                    error-buffer t))
            (progn
              (goto-char prev-point)
              (set-window-start nil prev-window-start)
              (kill-buffer error-buffer))
          ;; 1. `shell-command-on-region' with replacement first deletes the
          ;; buffer contents. This deletion goes into the `buffer-undo-list'.
          ;;
          ;; 2. At this point the command has returned failure, so nothing will
          ;; be put back into the buffer. Thus we undo the deletion.
          ;;
          ;; 3. However, simply undoing will also undo the last edit before this
          ;; function was run. To prevent that, first we run `undo-boundary'.
          ;;
          ;; TODO: Why in the world does it work to call `undo-boundary' here
          ;; instead of before the replacement?
          (undo-boundary)
          (undo)
          (error (format "shfmt: An error occurred when executing `%s'" shfmt-executable))))
    (error (format "shfmt: executable `%s' not found" shfmt-executable))))

(defun shfmt-buffer ()
  "Autoformat the current buffer."
  (interactive)
  (shfmt-region (point-min) (point-max)))

(provide 'shfmt)

;;; shfmt.el ends here
