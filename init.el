;; Bootstrap

(setq gc-cons-threshold (* 50 1000 1000));; Drop down again at the end of init.el

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(;;("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;; Bootstrap quelpa, then install quelpa-use-package which will pull
;; use-package in along for the ride. This replaces the explicit
;; bootstrapping of use-package.
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;;; Set up use-package
;; (eval-when-compile
;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))
;;   (setq use-package-always-ensure t)
;;   (require 'use-package)
;;   ;; Set use-package-compute-statistics to t to measure load stats;
;;   ;; review with M-x use-package-report
;;   (setq use-package-compute-statistics nil))

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load a file in current user's configuration directory"
  (interactive "f")
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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (setq mouse-sel-mode t))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Disable window-y stuff
(global-auto-revert-mode 1)
(menu-bar-mode -1)
(setq use-dialog-box nil)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)

(setq-default indent-tabs-mode nil)
(setq create-lockfiles nil)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; Set up windmove to bop around between windows quickly
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

;; Set font based on system type
(pcase system-type
  ;; Download: http://www.1001freefonts.com/roboto_mono.font
  ;; default font size (point * 10)
  ('darwin ((lambda ()
              (set-face-attribute 'default nil
                                  :font
                                  ;; "Roboto Mono for Powerline"
                                  ;; "Roboto Mono"
                                  "Inconsolata for Powerline"
                                  :height 120)
              (setq-default line-spacing 1))))
  ('gnu/linux (set-face-attribute 'default nil
                                  :font
                                  "Droid Sans Mono"
                                  ;; "Noto Sans Mono Light"
                                  :height 90)))

;; Set modifier keys on mac
(if (eq system-type 'darwin)
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'hyper)))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH"))
    (exec-path-from-shell-initialize)))

(defun add-auto-mode (mode &rest patterns)
  "E.g. (add-auto-mode 'web-mode \"*html*\" \"\\.ejs$\")"
  (mapc (lambda (pattern)
          (add-to-list 'auto-mode-alist (cons pattern mode)))
        patterns))

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

              ;; Split vertically a bit more often
              split-width-threshold 140
              split-height-threshold 180

              x-select-enable-clipboard t ;; makes killing/yanking interact with the clipboard
              x-select-enable-primary t)

;; Always clean up trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; or perhaps 'whitespace-cleanup

(set-language-environment "utf-8")

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 1))

(use-package fill-column-indicator
  :ensure t)

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
      desktop-auto-save-timeout 600
      desktop-load-locked-desktop t)  ;; don't worry about loading it over a lock
(desktop-save-mode 1)
(setq-default history-length 1000)

(use-package diminish)

;; Scratch buffer:
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "\
# Scratch

<!--
This buffer is for notes you don't want to save. If you want
to create a file, visit that file with C-x C-f then enter the
text in that file's own buffer.
-->

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

;; dired settings
(setq-default dired-listing-switches "-alh")

(use-package treemacs
  :pin melpa
  :bind ("C-x j d" . treemacs)
  :config (progn
            (if window-system
                (treemacs-resize-icons 16))
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode 'always)))

(use-package treemacs-projectile
  :pin melpa
  :after treemacs projectile
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :pin melpa
;;   :after treemacs dired
;;   :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :pin melpa
  :after treemacs magit
  :ensure t)

(use-package lsp-treemacs
  :after treemacs lsp
  :pin melpa)

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
  :demand;; force eager load
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("\r" . ivy-alt-done)  ;; [RET]
              ("C-j" . ivy-done)
              ("C-M-j" . ivy-immediate-done)
              ("S-SPC" . nil)
              ("M-SPC" . ivy-restrict-to-matches))
  :config
  (ivy-mode 1)
  ;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t  ;; E.g. for selecting a new input when a candidate matches
        ivy-wrap t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)) ;; This is really slow on big lists.
        ivy-initial-inputs-alist nil))

(use-package counsel
  :bind ("M-x" . counsel-M-x))

(use-package swiper
  :bind ("C-M-s" . swiper))

(use-package avy
  :bind (("M-g M-w" . avy-goto-char-2)
         ("M-g M-t" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.2))

(use-package projectile
  :defer 1
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-create-missing-test-files t)
  :config
  (projectile-global-mode))

(use-package ag
  :ensure t
  :config
  (setq ag-arguments '("--smart-case" "--stats" "--hidden")))

(use-package keychain-environment
  :demand
  :if (and
       (eq 'gnu/linux system-type)
       (locate-file "keychain" exec-path))
  :config
  (keychain-refresh-environment))

(use-package magit
  :defer t
  :pin melpa
  :bind ("C-c g" . magit-status)
  :config
  (setq-default magit-process-popup-time 10
                magit-diff-refine-hunk nil
                magit-auto-revert-mode t
                magit-display-buffer-function 'magit-display-buffer-traditional)
  ;; Make the current branch label more prominent in the log view:
  ;;(set-face-background 'magit-branch-current "#004020")
  ;;(set-face-foreground 'magit-branch-current "#de935f")
  (set-face-underline 'magit-branch-current t)
  (set-face-attribute 'magit-branch-current nil :weight 'bold))

(use-package gist
  :defer t)

(use-package git-gutter)

(use-package highlight-symbol
  :pin melpa
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)
         ("M-'" . highlight-symbol-query-replace))
  :hook ((prog-mode . highlight-symbol-mode)
         (html-mode . highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.75
        highlight-symbol-highlight-single-occurrence nil
        highlight-symbol-on-navigation-p t)
  (set-face-attribute 'highlight-symbol-face nil
                      :inherit 'default
                      :underline `(:color ,(face-attribute 'match :background))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config
  (setq dumb-jump-selector 'ivy)
  (dumb-jump-mode))

(use-package expand-region
  :defer t
  :commands (er/expand-region
             er/contract-region
             er/mark-inside-pairs
             er/mark-inside-quotes
             er/mark-outside-pairs
             er/mark-outside-quotes
             er/mark-defun
             er/mark-comment
             er/mark-text-sentence
             er/mark-text-paragraph
             er/mark-word
             er/mark-url
             er/mark-email
             er/mark-symbol))

(use-package company
  :config (progn (setq company-idle-delay 1)
                 (setq company-minimum-prefix-length 1))
  :commands (company-mode))

(use-package auto-complete
  :defer t
  :bind ("M-TAB" . auto-complete)
  :config (progn
            (require 'auto-complete-config)
            (setq-default ac-sources '(ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
            (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
            (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
            (add-hook 'css-mode-hook 'ac-css-mode-setup)
            (add-hook 'auto-complete-mode-hook 'ac-common-setup)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-more-like-this-extended)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes and appearance

(comment (use-package color-theme-sanityinc-tomorrow
           :demand t
           :config
           (load-theme 'sanityinc-tomorrow-bright t)))

(use-package monokai-theme
  :defer t
  :init
  (load-theme 'monokai t))

(use-package powerline
  :defer 1
  :config
  (ian/powerline-theme))

(defun ian/powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id
                           (if active
                               'mode-line-buffer-id
                             'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)

                                     (powerline-raw "%4l" face0 'l)
                                     (powerline-raw ":" face0 'l)
                                     (powerline-raw "%3c" face0 'r)

                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))

                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size face1 'l))

                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face0 face2))
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(global-hl-line-mode 0)

(show-paren-mode 1)
;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match "#a0a")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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

(use-package which-key
  :pin melpa-stable
  :diminish
  :config (which-key-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package avy-flycheck
  :defer t
  :bind (("M-g M-w" . avy-goto-char-2)
         ("M-g M-t" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.2))

(use-package flycheck
  :config
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (bind-keys
               :map flycheck-mode-map
               ("M-g M-e" . avy-flycheck-goto-error)))))

(use-package yasnippet
  ;; <TAB> expands a snippet
  :defer 1
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; (with-eval-after-load 'yasnippet
  ;;   (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (yas-reload-all)
  ;;(yas-global-mode)
  )

(use-package yasnippet-snippets
  ;; See http://andreacrotti.github.io/yasnippet-snippets/snippets.html for docs
  )

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

;; Embrace: for adding, changing and removing surrounding pairs.
(use-package embrace
  :config (global-set-key (kbd "C-,") #'embrace-commander))

;; (use-package eval-sexp-fu
;;   :ensure t)

(use-package jq-mode)  ;; M-x jq-interactively is awesome!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Server Protocol

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 1

        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 1

        lsp-ui-sideline-current-symbol '((t (:foreground "white" :weight ultra-bold :height 0.99)))
        lsp-ui-sideline-global '((t (:background "#004000")))
        lsp-ui-sideline-symbol '((t (:foreground "grey" :height 0.5)))

        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'bottom

        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog-mode config

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (git-gutter-mode)))

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
  :defer t
  :init (add-auto-mode 'clojure-mode "\\.edn$" "\\.boot$")
  :bind (:map clojure-mode-map
              (";" . sp-comment)
              ("C-c z" . cider-switch-to-repl-buffer))
  :config
  ;;(add-to-list 'auto-mode-alist '("\\.clj.*$" . clojure-mode))
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; This is useful for working with camel-case tokens, like names of
              ;; Java classes (e.g. JavaClassName)
              (subword-mode)
              (auto-complete-mode)
              (whitespace-mode)
              (set-fill-column ian/clojure-cols)
              (fci-mode)
              (setq fci-rule-column ian/clojure-cols
                    show-trailing-whitespace t)
              ;;(rainbow-delimiters-mode t)
              (smartparens-mode)
              (aggressive-indent-mode)
              (yas-minor-mode 1)

              ;; match: as in clojure.core.match
              (put-clojure-indent 'match 'defun)
              (put-clojure-indent 'fdef 'defun)
              (put-clojure-indent 'for-all 1))))

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :defer t
  :ensure t)

(use-package cider
  :pin melpa-stable
  :defer t
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

   cider-save-file-on-load t

   ;; Don't ask to jump to symbol
   cider-prompt-for-symbol nil))

(use-package cider-eval-sexp-fu
  :defer t)

(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; (use-package clj-refactor
;;   :defer t
;;   :hook clojure-mode
;;   :config
;;   (cljr-add-keybindings-with-prefix "C-c C-m")
;;   (setq cljr-favor-prefix-notation nil))

;;(add-hook 'cider-mode-hook #'eldoc-mode)  ;; this is terrible in CLJS

(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; Clojure inferior mode - e.g. for phantomjs
(use-package inf-clojure
  :defer t)

(defun cljs-start-phantom-repl ()
  (interactive)
  ;; TODO localization - remove the hard coded path
  (run-clojure (concat "lein trampoline run -m clojure.main "
                       (expand-file-name "~/.emacs.d/repls/phantom_repl.clj"))))

(use-package ac-cider
  :disabled  ;; <---------------
  :config
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (eval-after-load "auto-complete"
    '(progn
       (add-to-list 'ac-modes 'cider-mode)
       (add-to-list 'ac-modes 'cider-repl-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scss-mode - for SASS CSS files
(use-package scss-mode
  :defer t
  :config
  (setq scss-compile-at-save nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Javascript
(setq js-indent-level 2)
(add-hook 'js-mode-hook (lambda ()
                          (lsp)
                          (smartparens-mode)
                          (setq flycheck-idle-change-delay 1.0)
                          (when (flycheck-may-enable-checker 'javascript-eslint)
                            (flycheck-add-next-checker 'lsp 'javascript-eslint))))

(use-package prettier
  ;; global-prettier-mode only loads prettier for major modes it knows about
  :hook (after-init-hook . global-prettier-mode))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

;; Typescript
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . prettier-mode)
         (typescript-mode . smartparens-mode)
         ;;(typescript-mode . lsp)
         (typescript-mode . (lambda ()
                              (lsp)
                              ;; The chained eslint can get a bit weird because prettier
                              ;; reformats the buffer on save and there's a race.
                              ;; So, slow flycheck down:
                              (setq flycheck-idle-change-delay 1.0)
                              (when (flycheck-may-enable-checker 'javascript-eslint)
                                (flycheck-add-next-checker 'lsp 'javascript-eslint))))))

;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp))

(use-package mmm-mode
  :mode ("\\.vue\\'" "\\.vuex\\'")
  :init (setq mmm-global-mode 'maybe)
  :config
  (mmm-add-mode-ext-class 'web-mode "\\.vue\\'" 'javascript-mode))

;; PHP
(use-package php-mode
  :mode ("\\.php\\’" . php-mode)
  :hook (php-mode . (lambda ()
                      (prettier-mode)
                      (lsp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp

(add-hook 'lisp-mode-hook 'auto-complete-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'smartparens-mode)

(setq auto-mode-alist
      (append '(("\\.lisp$" . lisp-mode)
                ("\\.cl$" . lisp-mode))
              auto-mode-alist))

(use-package slime
  :defer t
  :pin melpa-stable
  :config
  (setq slime-lisp-implementations
        '((ccl ("/usr/local/bin/ccl64" "-K" "utf-8"))
          (sbcl ("/usr/local/bin/sbcl" "--noinform") :coding-system utf-8-unix)))
  (setq slime-default-lisp 'sbcl)
  ;; Add roswell if it's installed
  (if (file-exists-p "~/.roswell/helper.el")
      (progn
        (load (expand-file-name "~/.roswell/helper.el"))
        (setq slime-lisp-implementations
              (append '((roswell ("ros" "-Q" "run"))) slime-lisp-implementations))
        (setq slime-default-lisp 'roswell)))

  (add-hook 'slime-mode-hook 'smartparens-mode)
  (add-hook 'slime-repl-mode-hook 'smartparens-mode)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-contribs '(slime-asdf
                         slime-autodoc
                         slime-fancy
                         slime-highlight-edits
                         slime-quicklisp)))

(use-package ac-slime
  :defer t
  :pin melpa-stable
  :init
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  ;; :config
  ;; (progn
  ;;   (eval-after-load "auto-complete"
  ;;     '(add-to-list 'ac-modes 'slime-repl-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(use-package cc-mode
  :defer t
  :init (add-auto-mode 'c++-mode "\\.ino$")
  :hook ((c-mode . auto-complete-mode)
         (c++-mode . auto-complete-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang

(use-package go-mode
  :after (exec-path-from-shell)
  :init (add-auto-mode 'go-mode "\\.go$")
  :config (progn
            (exec-path-from-shell-copy-env "GOPATH")
            (exec-path-from-shell-copy-env "GOPRIVATE"))
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      ;;(add-hook 'before-save-hook 'gofmt-before-save nil 'local)
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map go-mode-map
              ("C-c C-c" . makefile-runner)
              ("M-." . godef-jump)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(setq ian/python-cols 88)

(add-hook 'python-mode-hook
          (lambda ()
            (setq mode-name "Py")  ;; "diminish" the major mode
            ;; Set up fill column indicator
            (set-fill-column ian/python-cols)
            ;; (fci-mode)
            ;; (setq fci-rule-column ian/python-cols
            ;;       show-trailing-whitespace t)
            (flycheck-mode)
            (setq python-shell-completion-native-enable nil)

            (pyenv-mode)

            (elpy-enable)
            (define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-sexp-safe)
            (define-key python-mode-map (kbd "C-M-b") 'python-nav-backward-sexp-safe)
            (define-key python-mode-map (kbd "C-c =") nil)

            (company-mode)
            (make-local-variable 'company-backends)
            ;;(add-to-list 'company-backends 'company-jedi)
            ))

;; Set up autocomplete
;; (use-package company-jedi)


(use-package elpy
  :commands (elpy-mode elpy-enable)
  :hook ((elpy-mode . flycheck-mode))

  :config
  (delete 'elpy-module-flymake elpy-modules)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (setq elpy-rpc-python-command "python3")

  :bind (:map elpy-mode-map
              ("C-c z" . elpy-shell-switch-to-shell)
              ("M-g M-n" . elpy-flymake-next-error)
              ("M-g M-p" . elpy-flymake-previous-error)
              ("C-c n" . elpy-black-fix-code)
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ;; ("C-c C-p" . nil)
              ;; ("C-c C-n" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)))

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (let ((pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
             (pyenv-mode-set pyenv-current-version)
             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; (use-package lsp-python-ms
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web stuff

(use-package web-mode
  :defer t
  :config
  (add-auto-mode 'web-mode
                 "*html*" "*twig*" "*tmpl*" "\\.erb" "\\.rhtml$" "\\.ejs$" "\\.hbs$"
                 "\\.ctp$" "\\.tpl$"
                 "\\.vue$" "\\.vuex$"
                 "/\\(views\\|html\\|templates\\)/.*\\.php$")
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package puppet-mode
  :defer t
  :pin melpa-stable)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;
;; Setup:
;; - install rustup:
;;   - https://rustup.rs/
;; - install Rust Analyzer binary:
;;     curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer && chmod +x ~/.local/bin/rust-analyzer
;;

(use-package flycheck-rust
  :defer t
  :commands flycheck-rust-setup)

(use-package flycheck-inline
  :hook flycheck)

(use-package rust-mode
  :defer t
  :after (lsp-mode)
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp)
         (rust-mode . smartparens-mode)
         (flycheck-mode . flycheck-rust-setup))
  :config (progn
            (setq rust-format-on-save t)
            (setq lsp-rust-server 'rust-analyzer)))

(use-package cargo
  :defer t
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil))

;; (use-package racer
;;   :defer t
;;   :commands racer-mode
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . eldoc-mode)
;;          (racer-mode . company-mode))

;;   :init
;;   (setq company-tooltip-align-annotations t)

;;   ;; Problems with racer having an incorrect RUST_SRC_PATH were fixed
;;   ;; with this setting, and by installing the rust sources with
;;   ;; `rustup component add rust-src`.
;;   (setq racer-rust-src-path
;;         (concat (string-trim
;;                  (shell-command-to-string "rustc --print sysroot"))
;;                 "/lib/rustlib/src/rust/src"))

;;   :config (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(use-package toml-mode
  :mode "\\.toml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobufs
(defconst ian/protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(use-package protobuf-mode
  :pin melpa-stable
  :hook (protobuf-mode . (lambda () (c-add-style "ian-protobuf-style" ian/protobuf-style t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup and text formatting

;; Spelling - from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;;
;; Find aspell and hunspell automatically
(cond
 ;; try hunspell at first, falling back to aspell then ispell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; The list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell.
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package markdown-mode
  :defer t
  :pin melpa-stable
  :hook (markdown-mode . (lambda ()
                           (turn-on-flyspell)
                           (flyspell-buffer)))
  :config (set-fill-column 78))

(use-package yaml-mode
  :defer t
  :pin melpa-stable)

;; (use-package highlight-indentation
;;   :defer t
;;   :hook ((web-mode . highlight-indent-guides-mode)
;;          (web-mode . highlight-indentation-current-column-mode)
;;          (html-mode . highlight-indent-guides-mode)
;;          (html-mode . highlight-indentation-current-column-mode)
;;          (yaml-mode . highlight-indentation-current-column-mode)))

(use-package graphviz-dot-mode
  :defer t)

;; Mermaid: for pretty pictures.
;;
;; Requires mmdc installed on command line, e.g. `brew install mermaid-cli`
;;
;;   C-c C-c - compile current file to an image
;;   C-c C-f - compile given file to an image
;;   C-c C-b - compile current buffer to an image
;;   C-c C-r - compile current region to an image
;;   C-c C-o - open in the live editor
;;   C-c C-d - open the official doc
(use-package mermaid-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infra

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile\\'"
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(use-package es-mode
  :defer t)

(use-package ob-http
  :defer t)

(use-package org
  :defer t

  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  ("C-c C-x C-j" . org-clock-goto)
  ("C-c c" . org-capture)

  :mode ("\\.org\\'" . org-mode)

  :hook (org-mode . yas-minor-mode)

  :config
  (load-user-file "modes/org.el")

  )

(use-package org-super-links
  :quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :commit "develop")
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-quick-insert-drawer-link)
         ("C-c s i" . org-super-links-quick-insert-inline-link)
         ("C-c s C-d" . org-super-links-delete-link))
  :config
  (setq org-super-links-related-into-drawer t
  	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Site-specific overrides
(load-all-in-directory "site")

;; TODO org-projectile
;; TODO org archiving - e.g. http://stackoverflow.com/a/35475878
;; DONE spacemacs multiple pane buffer wrapping -- it's `follow-mode`.
;; TODO window operations - vim-like would be nice for moving
;; TODO smartparens fun: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el, https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks
;; TODO multiple cursors
;; TODO expand region

(setq gc-cons-threshold (* 2 1000 1000))
(put 'narrow-to-region 'disabled nil)
