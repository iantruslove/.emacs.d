;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)

;;(load-theme 'tomorrow-night-bright t)

(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-bright)
