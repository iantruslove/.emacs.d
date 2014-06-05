(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(cl
                      cider
                      clj-refactor
                      clojure-mode
                      clojure-test-mode
                      color-theme-sanityinc-tomorrow
                      company ;; for cider autocompletion
                      dash
                      expand-region
                      ;; flx-ido
                      ;; flycheck
                      ;;flymake
                      ;;flymake-cursor
                      ;;flymake-jshint
                      ;; helm
                      ;; helm-ls-git
                      ;; helm-projectile
                      highlight-symbol
		      ido
                      js2-mode
                      js2-refactor
		      magit
		      paredit
                      pkg-info
                      ;; projectile
                      simp
                      slamhound
                      ;;starter-kit
                      ;;starter-kit-lisp
                      ;;starter-kit-bindings
                      markdown-mode
                      rainbow-delimiters
                      ;; smart-window
                      ;;skewer-mode
		      smex
                      ;;tree-mode
                      ;;yasnippet
                      ;;yas-jit
                      ;; zenburn-theme
                      zencoding-mode
                      ;;emacs-pry
                      )  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Indentation
(setq tab-width 2
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Suppress the startup screen
(setq inhibit-startup-screen t)

;; (load-theme 'wombat t)
;; (load-theme 'zenburn t)
(load-theme 'sanityinc-tomorrow-night t)

;; Shorter answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c q") 'join-line)

;; Yes, ido please
(ido-mode t)
(setq ido-enable-flex-matching t ;; enable fuzzy matching
      ido-use-virtual-buffers t)

;; smex is ido for the minibuffer
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Shortcut for magit-status
(global-set-key (kbd "C-c g") 'magit-status)

;; Disable backup files
(setq make-backup-files nil)

;; Always reload files changed from underneath
(global-auto-revert-mode t)

;; Show line and col number
(setq column-number-mode t)

;; Short alias for query-replace-regexp
(defalias 'qrr 'query-replace-regexp)

;; Enable highlight-symbol-mode
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-idle-delay 0.25) ;; 0.5 seconds
(global-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c h r") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c h q r") 'highlight-symbol-query-replace)

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

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Fire up Paredit when in various clojure modes
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'clojure-mode-hook 'clojure-test-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert clj-refactor mode keybinding setup here
                               ))

(add-hook 'paredit-mode-hook
	  (lambda ()
	    ;; For Joe
	    (define-key paredit-mode-map (kbd "C-o C-r") 'paredit-forward-slurp-sexp)
	    (define-key paredit-mode-map (kbd "C-o M-r") 'paredit-forward-barf-sexp)
	    (define-key paredit-mode-map (kbd "C-o C-l") 'paredit-backward-slurp-sexp)
	    (define-key paredit-mode-map (kbd "C-o M-l") 'paredit-backward-barf-sexp)

	    ;; For Daniel
	    (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
	    (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)

	    ;; Since Daniel's slurp bindings squash paredit-wrap-round...
	    (define-key paredit-mode-map (kbd "C-o C-w (") 'paredit-wrap-round)
	                (define-key paredit-mode-map (kbd "C-o C-w [") 'paredit-wrap-square)))

;; Convenient keybinding to switch to REPL, given C-z is the tmux
;; prefix key
(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c z") 'cider-switch-to-relevant-repl-buffer)
            ;;(define-key cider-mode-map (kbd "C-c M-n") 'nrepl-set-ns)
            ))

;; Always save when compiling
(setq cider-prompt-save-file-on-load nil)

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

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; >>> Configure Load Path <<<
;; From http://stackoverflow.com/a/1062314/594677
;; >>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emacs-config-path "~/.emacs.d/")
(setq base-lisp-path "~/.emacs.d/site-lisp/")
(setq site-lisp-path (concat emacs-config-path "/site-lisp"))
(defun add-path (p)
  (add-to-list 'load-path (concat base-lisp-path p)))

;; I should really just do this recursively.
(add-path "")
;; (add-path "some-nested-folder")

;; Load modules in site-lisp

(require 'transpose-frame)

(let ((sonian-stuff "~/projects/sa-safe/.elisp/sonian.el"))
  (when (file-exists-p sonian-stuff)
    (message "Loading Sonian extras...")
    (load (expand-file-name sonian-stuff))
    ;; Turn on whitespace mode all
    ;; the time
    (add-hook 'clojure-mode-hook 'whitespace-mode)))

;;; Load window management crap
;; (load "~/.emacs.d/emacsd-tile.el")

(menu-bar-mode -1)

;; Enable global auto-complete with company-mode:
(add-hook 'after-init-hook 'global-company-mode)

;; ClojureScript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; JavaScript
;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;(require 'flymake-jshint)
;;(require 'flymake-cursor)
;;(add-hook 'js2-mode-hook 'flymake-mode)

;; Load in skewer-mode
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)

;; JS2-refactor mode
(js2r-add-keybindings-with-prefix "C-c C-m")

;; Markdown support
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;;(electric-indent-mode +1)

;;(auto-fill-mode -1)

;; Enable winner mode - C-c <left> to go back a window configuration
(winner-mode)

;; Enable rainbow parens for all programming modes
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Custom keybindings
(global-set-key (kbd "M-@") 'er/expand-region) ; expand selected region

;; In-place scrolling
(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-down-line n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-up-line n))


;; Enable helm mode
;;(helm-mode t)

;; (global-set-key (kbd "C-x C-_")
;;                 ;; I have no idea why this matches C-x C-/, but it does and that's what I was after
;;                 (lambda ()
;;                   (interactive)
;;                   (helm :sources '(helm-source-findutils) :buffer "*helm-findutils*")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 4)
 '(js-indent-level 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq create-lockfiles nil)
