;;; amk-mac.el --- Functions specific to macOS -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Functions specific to macOS

;;; Code:

(require 'subr-x)

(defgroup amk-mac nil
  "Mac stuff"
  :group 'convenience)

(defun amk-mac-open-pwd ()
  "Open the current working directory in the Finder."
  (interactive)
  (let ((cmd (if buffer-file-name
                 (format "open -R %s" (shell-quote-argument (file-local-name buffer-file-name)))
               "open .")))
    (shell-command cmd)))

(defun amk-mac-open-file ()
  "Open the current file in the default app."
  (interactive)
  (if (buffer-file-name)
      (shell-command
       (format "open %s" (shell-quote-argument (file-local-name buffer-file-name))))
    (error "Buffer is not associated with a file")))

(defun amk-mac-reveal-file ()
  "Reveal the current file in the Finder."
  (interactive)
  (if (buffer-file-name)
      (shell-command
       (format "open -R %s" (shell-quote-argument (file-local-name buffer-file-name))))
    (error "Buffer is not associated with a file")))

(defun amk-mac-open-terminal ()
  "Open a macOS Terminal window in the pwd."
  (interactive)
  (shell-command "open -a Terminal ."))

(defun amk-mac-touch ()
  "Touch current buffer's file."
  (interactive)
  (when-let ((file (file-local-name buffer-file-name)))
    (shell-command (format "touch %s" (shell-quote-argument file)))
    (shell-command (format "date -r %s" (shell-quote-argument file)))))

(defun amk-mac--set-default (symbol value)
  "Set SYMBOL to VALUE."
  (cond ((eq symbol 'amk-mac-appearance-mode)
         (amk-mac-apply-appearance-mode value)))
  (set-default symbol value))

(defun amk-mac-apply-appearance-mode (mode)
  "Set Mac appearance MODE (light, dark, auto) on all frames."
  (dolist (frame (frame-list))
    (amk-mac--apply-appearance-mode-to-frame frame mode)))

(defun amk-mac--apply-appearance-mode-to-frame (frame mode)
  "Set Mac appearance MODE (light, dark, auto) on FRAME.

Auto only works on Yamamoto Mitsuharu's Mac port.

This does nothing if the variable `window-system' is nil (in a terminal)."
  (when window-system
    (with-selected-frame frame
      (cond ((eq mode 'light)
            (set-background-color "white")
            (set-foreground-color "black")
            (modify-frame-parameters frame '((ns-appearance . nil))))
           ((eq mode 'dark)
            (set-background-color "black")
            (set-foreground-color "white")
            (modify-frame-parameters frame '((ns-appearance . dark))))
           ((eq window-system 'mac)
            (set-background-color "mac:textBackgroundColor")
            (set-foreground-color "mac:textColor"))))
    (frame-set-background-mode frame)))

(defcustom amk-mac-appearance-mode nil
  "Mac appearance mode: \\='light, \\='dark, or nil (auto).

Auto only works on Yamamoto Mitsuharu's Mac port, not the NS port.

Programmatically set with `amk-mac-set-appearance-mode'."
  :group 'amk-mac
  :type '(choice (const :tag "Light Mode" light)
                 (const :tag "Dark Mode" dark)
                 (other :tag "Follow system" nil))
  :set #'amk-mac--set-default)

(defun amk-mac-toggle-appearance-mode ()
  "Toggle between light mode, dark mode, and automatic."
  (interactive)
  (let* ((mode amk-mac-appearance-mode)
         (next (cond ((eq mode 'light) 'dark)
                     ((eq mode 'dark) nil)
                     (t 'light))))
    (amk-mac--set-default 'amk-mac-appearance-mode next)
    (message "%s" (or next "auto"))))

(defun amk-mac--apply-current-appearance-mode-to-frame (frame)
  "Apply current theme to supplied FRAME."
  (amk-mac--apply-appearance-mode-to-frame frame amk-mac-appearance-mode))

(add-hook 'after-make-frame-functions #'amk-mac--apply-current-appearance-mode-to-frame)

(defcustom amk-mac-plist-mode-convert-format "xml1"
  "The format from which binary plists should be converted."
  :group 'amk-mac
  :type '(choice (const  "xml1")
                  (const "json")
                  (const "swift")
                  (const "objc")))


(defun amk-mac-convert-binary-plist (&optional buf)
  "Convert BUF or current from binary to something human-readable in a new buffer."
  ;; This approach used instead of `jka-compr' machinery due to issues described
  ;; here: https://hints.macworld.com/article.php?story=2005061422012079
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "bplist" t)
          (progn
            (shell-command-on-region
             (point-min)
             (point-max)
             (format "plutil -convert %s %s -o -"
                     amk-mac-plist-mode-convert-format
                     (shell-quote-argument (file-remote-p buffer-file-name 'localname)))))
        (error "Not a binary plist")))))

(provide 'amk-mac)
;;; amk-mac.el ends here
