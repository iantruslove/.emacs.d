;; Show matching paren
(require 'paren)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Configure matching paren highlight coloring now the theme is loaded
;; It seems 
(defun modify-paren-match-colors ()
  (set-face-background 'show-paren-match-face (face-background 'default))
  (set-face-attribute 'show-paren-match nil :foreground nil
                      :background "#1a1a1a" :weight 'normal))

(add-hook 'window-setup-hook 'modify-paren-match-colors)
