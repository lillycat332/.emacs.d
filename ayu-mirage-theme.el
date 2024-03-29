(deftheme ayu-mirage
  "An emacs port of the excellent Ayu Mirage theme.")

(custom-theme-set-faces
 'ayu-mirage
 '(cursor ((t (:background "#FFCC66"))))
 '(fixed-pitch ((t (:extend nil :background "#242936" :foreground "#CCCAC2" :family "Liga SFMono Nerd Font"))))
 '(variable-pitch ((t (:background "#242936" :family "SF Pro Text"))))
 '(escape-glyph ((t (:background "#242936" :foreground "#FFDFB3"))))
 '(minibuffer-prompt ((t (:background "#242936" :foreground "#767c85"))))
 '(highlight ((t (:background "#2b4768"))))
 '(region ((t (:background "#2b4768"))))
 '(shadow ((t (:background "#242936" :foreground "#767c85"))))
 '(secondary-selection ((t (:background "#283850"))))
 '(trailing-whitespace ((((class color) (background light))
	     (:background "red1"))
	    (((class color)
	      (background dark))
	     (:background "red1"))
	    (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:background "#242936"
		   :foreground "#5ccfe6"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:background "#242936" :foreground "#6e7c8e"))))
 '(font-lock-constant-face ((t (:background "#242936" :foreground "#dfbfff"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-function-name-face ((t (:background "#242936" :foreground "#ffd173"))))
 '(font-lock-keyword-face ((t (:background "#242936" :foreground "#ffad66" :weight bold))))
 '(font-lock-preprocessor-face ((t (:background "#242936" :foreground "#f28779"))))
 '(font-lock-regexp-grouping-backslash ((t (:background "#242936" :foreground "#95e6cb" :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:background "#242936" :foreground "#95e6cb" :weight bold))))
 '(font-lock-string-face ((t (:background "#242936" :foreground "#d5ff80" :weight bold))))
 '(font-lock-type-face ((t (:background "#242936" :foreground "#73d0ff" :weight bold))))
 '(font-lock-variable-name-face ((t (:background "#242936" :foreground "#73d0ff"))))
 '(font-lock-warning-face ((t (:background "#242936" :foreground "#ff6666" :weight bold))))
 '(button ((t (:inherit link :background "#242936"))))
 '(link ((t (:background "#242936" :foreground "#61AFEF" :underline nil :weight bold))))
 '(link-visited ((t (:background "#242936" :foreground "#61AFEF" :underline nil :weight normal))))
 '(fringe ((t (:background "#242936"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(tooltip ((t (:inherit variable-pitch :background "lightyellow" :foreground "#242936"))))
 '(mode-line ((t (:background "#242936" :foreground "#9DA5B4" :box (:line-width 1 :color "#181A1F")))))
 '(mode-line-buffer-id ((t (:background "#242936" :weight bold))))
 '(mode-line-emphasis ((t (:background "#242936" :weight bold))))
 '(mode-line-highlight ((t (:inherit highlight))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "#181A1F" :style nil) :foreground "#3E4451" :background "#181A1F"))))
 '(isearch ((t (:foreground "#282C34" :background "#C678DD"))))
 '(isearch-fail ((t (:foreground "#BE5046"))))
 '(lazy-highlight ((t (:underline (:color "#C678DD" :style line) :foreground "#C678DD" :background "#121417"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1"))
          (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
          (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
          (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
          (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(default ((t (:stipple nil
	     :background "#242936"
	     :foreground "#CCCAC2"
	     :weight normal
	     :family "Liga SFMono Nerd Font"))))
 '(tab-line-tab ((t (:height 135
	         :padding-right 20
	         :padding-left 20
	         :background "#1A1F29"
	         :box (:line-width 5 :color "#1A1F29")
	         :weight bold))))
 '(tab-line-tab-current ((t (:inherit mode-line
	                  :height 135
	                  :box nil
	                  :background "#242936"
	                  :box (:line-width 5 :color "#242936")
	                  :weight bold))))
 '(tab-line-tab-inactive ((t (:background "#1A1F29"
		  :weight bold
		  :box (:line-width 5 :color "#1A1F29")))))
 '(tab-line-tab-inactive-alternate ((t (:background "#1A1F29"
		            :box (:line-width 5 :color "#1A1F29")))))
 '(tab-line-highlight ((t (:background "#1A1F29"))))
 '(tab-line-tab-special ((t (:italic t))))
 '(tab-line ((t (:height 135
	     :padding-left 20
	     :padding-right 20
	     :background "#1A1F29"
	     :box nil)))))

(provide-theme 'ayu-mirage)
