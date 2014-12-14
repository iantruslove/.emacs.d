(require 'projectile)
(require 'grizzl)

(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

;; Requiring ag seems to fix `C-c p A` functionality
;;(require 'ag)

;; I have no idea why this matches C-c C-/, but it does and that's what I was after
(global-set-key (kbd "C-c C-_") 'projectile-find-file)
(global-set-key (kbd "C-c C-b") 'projectile-switch-to-buffer)
