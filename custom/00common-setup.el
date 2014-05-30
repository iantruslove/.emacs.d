;; Indentation
(setq tab-width 2
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

;; Configure scrolling a little more to my liking
(setq
 scroll-margin 2
 scroll-conservatively 100000
 scroll-preserve-screen-position 1
 next-screen-context-lines 4)

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

;; Globally enable company-mode for autocomplete
(add-hook 'after-init-hook 'global-company-mode)

;; Disable backup files
(setq make-backup-files nil)

;; Always reload files changed from underneath
(global-auto-revert-mode t)

;; Show line and col number
(setq column-number-mode t)

;; Short alias for query-replace-regexp
(defalias 'qrr 'query-replace-regexp)

;; Enable silver searcher highlighting
(setq ag-highlight-search t)

;; Enable highlight-symbol-mode
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-idle-delay 0.5) ;; 0.5 seconds
(global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c h r") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c h q r") 'highlight-symbol-query-replace)

;; Expand-region
(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c -") 'er/contract-region)

;; Gitgutter mode
(global-git-gutter-mode +1)

;; Shortcut for magit-status
(global-set-key (kbd "C-c g") 'magit-status)

;; Make code purdy, stolen from larkin and ESK
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (if (< (buffer-size) 100000)
      (progn
        (indent-buffer)
        (untabify-buffer)
        (delete-trailing-whitespace)
        (message "Cleaned up buffer."))
    (message "Didn't clean up buffer.")))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Disable arrow keys!
(require 'no-easy-keys)
(no-easy-keys 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
