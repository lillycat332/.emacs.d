;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; Add MELPA package repo
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Set theme
(load-theme 'atom-one-dark t)

;; Enable dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Dashboard config
;; Set the title
(setq dashboard-banner-logo-title "(λ Emacs)")
(setq dashboard-startup-banner "~/.emacs.d/blk-hole.png")

;; Enable init info
(setq dashboard-set-init-info t)
;; ;; Use cool emacs logo instead of the GNU one
;; (setq dashboard-startup-banner 'logo)
;; Custom footer
(setq dashboard-footer-messages '("Have a nice day!"))
(setq dashboard-footer-icon
      (all-the-icons-octicon "heart"
                             :height 1.1
                             :v-adjust -0.05
                             :face 'font-lock-keyword-face))
;; Center content
(setq dashboard-center-content t)

;; Use dashboard as initial buffer.
(setq initial-buffer-choice
      (lambda () (get-buffer-create "*dashboard*")))

;; Use doom modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Set height for modeline
(setq-default doom-modeline-height 30)

;; enable discord rpc
(require 'elcord)
(elcord-mode)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Use transient mark
(transient-mark-mode 1)

;;; Org mode configuration
;; Enable Org mode
(require 'org)

;; enable web mode for jsx
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; enable 80 col ruler
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; enable line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use bar cursor
(setq-default cursor-type 'bar)

;; disable tilde file stuff
(setq backup-directory-alist `(("." . "~/.emacs.bak")))

;; Enable racket xp minor mode
(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)

;; Enable code completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'racket-mode-hook 'company-mode)
(add-hook 'racket-repl-mode-hook 'company-mode)
(add-to-list 'company-backends #'company-tabnine)

(use-package fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; Disable tool bar
(tool-bar-mode -1)

;; Set the title bar to black
(defun set-selected-frame-dark (&rest _)
  (call-process-shell-command "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -id \"$(xdotool getactivewindow)\""))

;; Make titlebar dark on linux gtk
(when (eq system-type 'gnu/linux)
  (add-hook 'after-make-frame-functions #'set-selected-frame-dark))

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'vterm)
(defun vterm-exec (xs)
  "Execute string xs in vterm."
  (progn (vterm-other-window)
	 (set-process-sentinel vterm--process #'run-in-vterm-kill)
	 (vterm-send-string xs)
	 (vterm-send-return)))

(defun racket-racket ()
  "Do \"racket <file>\" in a shell buffer."
  (interactive)
  (vterm-exec (concat (shell-quote-argument racket-program)
                         " "
                         (shell-quote-argument (racket--buffer-file-name)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" default))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil
		  :stipple nil
		  :background "#000919"
		  :foreground "#c3c0bb"
		  :inverse-video nil
		  :box nil
		  :strike-through nil
		  :extend nil
		  :overline nil
		  :underline nil
		  :slant normal
		  :weight normal
		  :height 120
		  :width normal
		  :foundry "nil"
		  :family "Liga SFMono Nerd Font"))))
 
 '(line-number ((t (:inherit (shadow default) :background "gray7"))))
 
 '(line-number-current-line ((t (:foreground "VioletRed1"
				 :inherit line-number)))))
