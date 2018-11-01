;;; shfmt.el --- Autoformat shell scripts -*- lexical-binding: t -*-

;; Shell script autoformatting using shfmt; see https://github.com/mvdan/sh

;; Package-Requires: (cl-lib)

;;; Commentary:

;;; Code:

(defcustom shfmt-executable "shfmt" "The executable to run when autoformatting."
  :group 'shfmt
  :type '(string))

(defcustom shfmt-arguments "" "The args to supply to `shfmt-executable' when autoformatting."
  :group 'shfmt
  :type '(string)
  :safe #'stringp)

(defvar shfmt--debug nil)

(defun shfmt-build-argument-list ()
  "Build a list of arguments to `shfmt-executable' based on \
`shfmt-arguments' and user settings."
  (let ((indent (when (boundp 'sh-basic-offset)
                  `("-i" ,(number-to-string sh-basic-offset)))))
    `(,@indent ,@(split-string shfmt-arguments))))

(defun shfmt-build-command ()
  "Build the command to execute when autoformatting."
  (mapconcat #'identity `(,shfmt-executable ,@(shfmt-build-argument-list)) " "))

(defun shfmt-enable-on-save ()
  "Pre-save hook for running shfmt."
  (interactive)
  (add-hook 'before-save-hook 'shfmt-buffer nil t))

(defun shfmt-region (start end)
  "Autoformat the region defined by START and END."
  (interactive)
  (if (executable-find shfmt-executable)
      (let ((args (shfmt-build-argument-list)))
        (if (member "-d" args)
            (shfmt--patch-region start end args)
          (shfmt--replace-region start end (shfmt-build-command))))
    (error (format "shfmt: executable `%s' not found" shfmt-executable))))

(defun shfmt--replace-region (start end cmd)
  "Replace region from START to END with the result of executing CMD."
  (let* ((prev-point (point))
         (prev-window-start (window-start))
         (error-buffer-name "*Shfmt Errors*")
         (error-buffer (get-buffer-create error-buffer-name))
         (coding-system-for-write buffer-file-coding-system)
         (coding-system-for-read buffer-file-coding-system)
         (show-errors (not (flycheck-shfmt-in-use-p))))
    (if (zerop (shell-command-on-region
                start end
                cmd
                (current-buffer) t
                error-buffer show-errors))
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
      (error (format "shfmt: An error occurred when executing `%s'" shfmt-executable)))))

(defun shfmt--patch-region (start end args)
  "Patch the region from START to END with the diff obtained by executing shfmt with ARGS."
  (let* ((patch-buffer-name "*Shfmt Patch*")
         (patch-buffer (get-buffer-create patch-buffer-name))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (call-process-args `(,start ,end ,shfmt-executable ,nil ,patch-buffer ,t ,@args)))
    (with-current-buffer patch-buffer
      (erase-buffer))
    (when (= 1 (apply #'call-process-region call-process-args))
        (save-excursion
          (shfmt--apply-patch patch-buffer start-line end-line (current-buffer))
          (kill-buffer patch-buffer)))))

(defun shfmt--apply-patch (patch-buffer start-line end-line target-buffer)
  "Apply patch in PATCH-BUFFER to region from START-LINE to END-LINE in TARGET-BUFFER."
  (with-current-buffer patch-buffer
    (goto-char 0)
    (cl-labels ((current-line ()
                              (buffer-substring-no-properties
                               (1+ (line-beginning-position))
                               (line-beginning-position 2))))
      (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\)" nil t)
       (let* ((hunk-start (string-to-number (match-string 1)))
              (hunk-start-adj (+ start-line hunk-start -1))
              (offset 0))
         (if shfmt--debug
             (message "Found hunk: %s" (match-string 0)))
         (while (progn
                  (forward-line 1)
                  (cond ((looking-at "^ ")
                         (if shfmt--debug
                             (message "Context line"))
                         (setq offset (1+ offset)))
                        ((looking-at "^\\+")
                         (let ((add-line (current-line)))
                           (if shfmt--debug
                               (message "Add line at %d: %s"
                                        (+ offset hunk-start-adj)
                                        add-line))
                           (with-current-buffer target-buffer
                             (let ((add-line-num (+ offset (- hunk-start-adj (line-number-at-pos)))))
                               (forward-line add-line-num)
                               (insert add-line)))
                           (setq offset (1+ offset))
                           t))
                        ((looking-at "^\\-")
                         (let ((del-line (current-line)))
                           (if shfmt--debug
                               (message "Delete line at %d: %s"
                                        (+ offset hunk-start-adj)
                                        del-line))
                           (with-current-buffer target-buffer
                             (let ((del-line-num (+ offset (- hunk-start-adj (line-number-at-pos)))))
                               (forward-line del-line-num)
                               (delete-region (line-beginning-position) (line-beginning-position 2))))
                           t))))))))))

(defun shfmt-buffer ()
  "Autoformat the current buffer."
  (interactive)
  (shfmt-region (point-min) (point-max)))

(defun flycheck-shfmt-in-use-p ()
  "Return non-nil if flycheck-shfmt is in use in the current buffer."
  (let ((fun 'flycheck-may-use-checker))
    (when (functionp fun)
      (funcall fun 'sh-shfmt))))

(provide 'shfmt)

;;; shfmt.el ends here
