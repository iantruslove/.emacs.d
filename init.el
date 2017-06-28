;; Bootstrap

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(defun load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  ;; TODO: handle if dir doesn't exist
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files (expand-file-name dir user-init-dir) t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental Emacs config

(global-auto-revert-mode 1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(when (eq system-type 'darwin)
  ;; Download: http://www.1001freefonts.com/roboto_mono.font
  (set-face-attribute 'default nil :family "Roboto Mono")
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :height 120))

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(setq-default column-number-mode t
              custom-file (expand-file-name "site/emacs-custom.el" user-init-dir) ;; Saves emacs customizations to somewhere other than the end of init.el
              delete-selection-mode t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              inhibit-splash-screen t
              make-backup-files nil
              require-final-newline 'visit-save
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              x-select-enable-clipboard t ;; makes killing/yanking interact with the clipboard
              x-select-enable-primary t)

(set-language-environment "utf-8")

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 1))

(use-package fill-column-indicator
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ibuffer-projectile
  :config
  (setq projectile-enable-caching t)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Desktops and sessions
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)
(setq-default history-length 1000)
(savehist-mode t)
(setq desktop-globals-to-save
      ;; TODO: There's surely a bunch of cruft in here to clean up
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                tags-table-list)))

(use-package session
  :config
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (add-hook 'after-init-hook 'session-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$" ".*png$" ".*cache$")
        recentf-max-saved-items 60))

(use-package smex
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package flx
  :ensure t)

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  ;;(global-set-key (kbd "C-c C-r") 'ivy-resume) ;; interesting. TODO put it into a hydra
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ;;ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)) ;; This is really slow on big lists.
        ivy-initial-inputs-alist nil))

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package swiper
  :config
  (global-set-key (kbd "C-M-s") 'swiper))

(use-package hydra
  :ensure t)

(load-user-file "hydras/smartparens.el")

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy
        projectile-create-missing-test-files t))

(use-package ag
  :ensure t)

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  (setq-default magit-process-popup-time 10
                magit-diff-refine-hunk nil
                magit-auto-revert-mode t))

(use-package gist
  :ensure t)

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)
         ("M-'" . highlight-symbol-query-replace))
  :config
  (setq highlight-symbol-idle-delay 0.75)
  (dolist (hook '(prog-mode-hook html-mode-hook))
    (add-hook hook (lambda ()
                     (highlight-symbol-mode)))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config (setq dumb-jump-selector 'ivy))

(dumb-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes and appearance

(use-package color-theme-sanityinc-tomorrow
  :demand t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(global-hl-line-mode 1)
(show-paren-mode 1)

(setq whitespace-style '(face trailing tabs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor

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

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package aggressive-indent
  :ensure t)

(use-package smartparens
  :config
  ;; load up smartparens default config - https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el
  (require 'smartparens-config)

  (bind-keys
   :map smartparens-mode-map
   ("C-k"         . sp-kill-hybrid-sexp)
   ("C-M-k"       . sp-kill-sexp)
   ("<backspace>" . sp-backward-delete-char)
   ("C-M-i"       . smartparens-hydra/body)
   ;; ("C-M-a" . sp-beginning-of-sexp)
   ;; ("C-M-e" . sp-end-of-sexp)

   ;; ("C-<down>" . sp-down-sexp)
   ;; ("C-<up>"   . sp-up-sexp)
   ;; ("M-<down>" . sp-backward-down-sexp)
   ;; ("M-<up>"   . sp-backward-up-sexp)

   ;; ("C-M-f" . sp-forward-sexp)
   ;; ("C-M-b" . sp-backward-sexp)

   ;; ("C-M-n" . sp-next-sexp)
   ;; ("C-M-p" . sp-previous-sexp)

   ;; ("C-S-f" . sp-forward-symbol)
   ;; ("C-S-b" . sp-backward-symbol)

   ("M-)" . sp-forward-slurp-sexp)
   ("C-M-)" . sp-forward-barf-sexp)
   ("M-("  . sp-backward-slurp-sexp)
   ("C-M-("  . sp-backward-barf-sexp)

   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-raise-sexp)
   ("C-M-r" . sp-splice-sexp-killing-backward)

   ;; ("C-M-t" . sp-transpose-sexp)
   ;; ("C-M-k" . sp-kill-sexp)
   ;; ("C-k"   . sp-kill-hybrid-sexp)
   ;; ("M-k"   . sp-backward-kill-sexp)
   ;; ("C-M-w" . sp-copy-sexp)

   ;; ("C-M-d" . delete-sexp)

   ;; ("M-<backspace>" . backward-kill-word)
   ;; ("C-<backspace>" . sp-backward-kill-word)
   ;; ([remap sp-backward-kill-word] . backward-kill-word)

   ;; ("M-[" . sp-backward-unwrap-sexp)
   ;; ("M-]" . sp-unwrap-sexp)

   ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

   ;; ("C-c ("  . wrap-with-parens)
   ;; ("C-c ["  . wrap-with-brackets)
   ;; ("C-c {"  . wrap-with-braces)
   ;; ("C-c '"  . wrap-with-single-quotes)
   ;; ("C-c \"" . wrap-with-double-quotes)
   ;; ("C-c _"  . wrap-with-underscores)
   ;; ("C-c `"  . wrap-with-back-quotes)
   )
  )

;; (use-package eval-sexp-fu
;;   :ensure t)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
(put 'use-package 'lisp-indent-function 1)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (smartparens-mode)
                                  (aggressive-indent-mode)
                                  (bind-keys
                                   :map emacs-lisp-mode-map
                                   (";" . sp-comment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(setq ian/clojure-cols 80)

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.clj.*$" . clojure-mode))
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; This is useful for working with camel-case tokens, like names of
              ;; Java classes (e.g. JavaClassName)
              (bind-keys
               :map clojure-mode-map
               (";" . sp-comment)
               ("C-c z" . cider-switch-to-repl-buffer))
              (subword-mode)
              (auto-complete-mode)
              (whitespace-mode)
              (set-fill-column ian/clojure-cols)
              (fci-mode)
              (setq fci-rule-column ian/clojure-cols
                    show-trailing-whitespace t)
              ;;(rainbow-delimiters-mode t)
              (smartparens-mode)
              (aggressive-indent-mode))))

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :pin melpa-stable
  :config
  (setq
   ;; go right to the REPL buffer when it's finished connecting
   cider-repl-pop-to-buffer-on-connect t

   ;; Don't show the REPL welcome message
   cider-repl-display-help-banner nil 

   ;; When there's a cider error, don't show it.
   cider-show-error-buffer nil
   cider-auto-select-error-buffer nil

   ;; Where to store the cider history.
   cider-repl-history-file "~/.emacs.d/cider-history"
   cider-repl-history-size 5000

   ;; Wrap when navigating history.
   cider-repl-wrap-history t

   cider-prompt-save-file-on-load 'always-save

   ;; Don't ask to jump to symbol
   cider-prompt-for-symbol nil))

(use-package cider-eval-sexp-fu
  :ensure t)

(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(use-package clj-refactor
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'cider-mode-hook #'eldoc-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (yas-minor-mode 1)))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook
             (lambda ()
               (progn
                 ;; match: as in clojure.core.match
                 (put-clojure-indent 'match 'defun)))))

(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; Clojure inferior mode - e.g. for phantomjs
(use-package inf-clojure
  :ensure t)

(defun cljs-start-phantom-repl ()
  (interactive)
  ;; TODO localization - remove the hard coded path
  (run-clojure (concat "lein trampoline run -m clojure.main "
                       (expand-file-name "~/.emacs.d/repls/phantom_repl.clj"))))
(use-package ac-cider
  :config
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (eval-after-load "auto-complete"
    '(progn
       (add-to-list 'ac-modes 'cider-mode)
       (add-to-list 'ac-modes 'cider-repl-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp

(add-hook 'lisp-mode-hook 'smartparens-mode)
(add-hook 'lisp-mode-hook #'eldoc-mode)

(setq auto-mode-alist
      (append '(("\\.lisp$" . lisp-mode)
                ("\\.cl$" . lisp-mode))
              auto-mode-alist))

(setq slime-default-lisp 'ccl)
(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64" "-K" "utf-8"))))

(use-package slime
  :pin melpa-stable
  :config
  (add-hook 'slime-mode-hook 'smartparens-mode)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(use-package elpy
  :config
  (elpy-enable)
  ;;(setq elpy-rpc-backend "jedi")
  ;;(setq elpy-rpc-python-command "python3.5")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web stuff

(defun add-auto-mode (mode &rest patterns)
  (mapc (lambda (pattern)
          (add-to-list 'auto-mode-alist (cons pattern mode)))
        patterns))

(use-package web-mode
  :config
  (add-auto-mode 'web-mode
                 "*html*" "*twig*" "*tmpl*" "\\.erb" "\\.rhtml$" "\\.ejs$" "\\.hbs$"
                 "\\.ctp$" "\\.tpl$"
                 "/\\(views\\|html\\|templates\\)/.*\\.php$")
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package php-mode
  :init
  (add-hook 'php-mode-hook
            (lambda ()
              (setq c-basic-offset 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom modules
(load-user-file "modes/org.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Site-specific overrides
(load-all-in-directory "site")

;; TODO org-projectile
;; TODO org archiving - e.g. http://stackoverflow.com/a/35475878
;; DONE spacemacs multiple pane buffer wrapping -- it's `follow-mode`.
;; TODO: set up some hydras
;; - clj-refactor
;; - cider
;; - window movements and rearrangements
;; TODO window operations - vim-like would be nice for moving
;; TODO smartparens fun: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el, https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks
;; TODO multiple cursors
;; TODO expand region
