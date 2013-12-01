;; Indentation
(setq tab-width 4
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Suppress the startup screen
(setq inhibit-startup-screen t)

;; Shorter answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c q") 'join-line)

(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; Hide menus
(menu-bar-mode -1)

;; Set up ibuffer to replace the buffer list
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enable smex for better M-x love
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Enable ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Disable backup files
(setq make-backup-files nil)

;; Enable silver searcher highlighting
(setq ag-highlight-search t)

;; Show matching paren
(require 'paren)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#000")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
(show-paren-mode 1)

;; Expand-region
(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c -") 'er/contract-region)
