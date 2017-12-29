;;; amk-edit.el --- Custom editing commands

;;; Commentary:

;;; Code:

;; Eclipse-like line moving
;; https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest body)
  "Note current column, do BODY, then restore column."
  (let ((tmp (make-symbol "col")))
    `(let ((,tmp (current-column)))
       ,@body
       (move-to-column ,tmp))))

(defun move-lines-up ()
  "Move the current line or region up by one line."
  (interactive)
  (save-current-buffer
    (if (use-region-p)
        (move-lines-impl t)
      (move-line-up))))

(defun move-lines-impl (&optional up)
  "Move the current region up by one line.  Set UP to true to go up."
  (let (deactivate-mark
        (rstart (region-beginning))
        (rend (region-end)))
    (goto-char rstart)
    (let ((start (line-beginning-position)))
      (goto-char rend)
      (let* ((end (if (= (current-column) 0)
                         rend
                         (line-beginning-position 2)))
             (text (buffer-substring start end)))
        (delete-region start end)
        (forward-line (if up -1 1))
        (push-mark)
        (insert text)))))

(defun move-line-up ()
  "Swap the current line with the previous one."
  (interactive)
  (save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun move-lines-down ()
  "Move the current line or region down by one line."
  (interactive)
  (save-current-buffer
    (if (use-region-p)
        (move-lines-impl)
      (move-line-down))))

(defun move-line-down ()
  "Swap the current line with the next one."
  (interactive)
  (save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(provide 'amk-edit)
;;; amk-edit.el ends here
