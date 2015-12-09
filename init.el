;; Increase default font size.
(set-face-attribute 'default nil :height 180)

;; Set decent default fonts for Japanese and Chinese,
;; but *only* if in a graphical context.
;; Set Japanese second so that Japanese glyphs override Chinese
;; when both charsets cover the same codepoints.
(if (fboundp 'set-fontset-font)
    (progn
      (set-fontset-font
       nil
       'chinese-gbk
       (font-spec :family "Hiragino Sans GB W3"))
      (set-fontset-font
       nil
       'japanese-jisx0213.2004-1
       (font-spec :family "Hiragino Kaku Gothic ProN"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(ns-pop-up-frames nil)
 '(org-agenda-files (quote ("~/Documents/org/agenda/")))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ensure emacs shell has regular shell environment
;; via exec-path-from-shell package.
(package-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Set up spelling
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq-default indent-tabs-mode nil)

(show-paren-mode)
(column-number-mode)

;; Only on GUI
(if window-system
    (progn
      ;; Restore session
      (desktop-save-mode t)
      ;; Disable C-z (suspend-frame) in GUI because it's pointless
      ;; and I keep hitting it.
      (global-unset-key "\C-z")
      ;; Run server so `emacsclient` will edit in GUI editor.
      ;; May have to prepend /Applications/Emacs.app/Contents/MacOS/bin
      ;; to PATH on OS X.
      (server-start)
      ;; ediff customizations
      ;; https://www.ogre.com/node/446
      (add-hook 'ediff-before-setup-hook
                'new-frame)
      (add-hook 'ediff-quit-hook
                'delete-frame)
      (add-hook 'ediff-startup-hook
                '(lambda ()
                   (set-frame-size (selected-frame) 175 55)
                   (raise-frame (selected-frame))))))

;; Don't intercept Japanese IME controls
(global-unset-key (kbd "C-S-j"))
(global-unset-key (kbd "C-:"))

;; Set magit shortcut
(global-set-key (kbd "C-x g") 'magit-status)

;; Add melpa package repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Add JDEE Java development environment
(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
(load "jde")

;; Improve text printing speed in Python shell
(setq python-shell-enable-font-lock nil)

;; Wrap lines in org-mode
(setq org-startup-truncated nil)

;; Backlog link support in org-mode
(load "~/.emacs.d/org-backlog.el")
(setq org-backlog-team "rxdev")

;; Set org-agenda stuff
(global-set-key (kbd "C-c a") 'org-agenda)

