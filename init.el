;; Bootstrap

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;; Set up use-package

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq use-package-always-ensure t)

  (require 'use-package)

  ;; Set use-package-compute-statistics to t to measure load stats;
  ;; review with M-x use-package-report
  (setq use-package-compute-statistics nil))

;; For the code use-package writes, but we're not always loading use-package:
(require 'diminish)
(require 'bind-key)


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

(pcase system-type
  ;; Download: http://www.1001freefonts.com/roboto_mono.font
  ;; default font size (point * 10)
  ('darwin ((lambda ()
              (set-face-attribute 'default nil
                                  :font "Roboto Mono"
                                  :height 120)
              (setq-default line-spacing 2))))
  ('gnu/linux (set-face-attribute 'default nil
                                  :font "Liberation Mono"
                                  :height 100)))

(when (memq window-system '(mac ns))
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

;; (use-package auto-compile
;;   :config
;;   (setq load-prefer-newer t)
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; or perhaps 'whitespace-cleanup

(set-language-environment "utf-8")

;; Spelling - from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;;
;; find aspell and hunspell automatically
(cond
 ;; try hunspell at first
 ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          )))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))


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
      desktop-auto-save-timeout 600
      desktop-load-locked-desktop t)  ;; don't worry about loading it over a lock
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
  :demand;; force eager load
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("\r" . 'ivy-alt-done)  ;; [RET]
              ("C-j" . 'ivy-done)
              ("S-SPC" . nil)
              ("M-SPC" . 'ivy-restrict-to-matches))
  :config
  (ivy-mode 1)
  ;;(global-set-key (kbd "C-c C-r") 'ivy-resume) ;; interesting. TODO put it into a hydra
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

(use-package hydra
  :config
  (load-user-file "hydras/smartparens.el"))

(use-package projectile
  :demand
  :diminish projectile-mode
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-create-missing-test-files t)
  :config
  (projectile-global-mode))

(use-package ag
  :ensure t)

(use-package magit
  :defer t
  :bind ("C-c g" . magit-status)
  :config
  (setq-default magit-process-popup-time 10
                magit-diff-refine-hunk nil
                magit-auto-revert-mode t))

(use-package gist
  :defer t)

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)
         ("M-'" . highlight-symbol-query-replace))
  :config
  (setq highlight-symbol-idle-delay 0.75)
  (setq highlight-symbol-highlight-single-occurrence nil)
  (setq highlight-symbol-on-navigation-p t)
  (dolist (hook '(prog-mode-hook html-mode-hook))
    (add-hook hook (lambda ()
                     (highlight-symbol-mode)))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config
  (setq dumb-jump-selector 'ivy)
  (dumb-jump-mode))

(use-package expand-region
  :bind ("C-c =" . er/expand-region))

(use-package auto-complete
  :config
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))


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

(use-package which-key
  :pin melpa-stable
  :config (which-key-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package avy-flycheck
  :defer t
  :config
  (global-set-key (kbd "M-g M-w") 'avy-goto-char-2)
  (setq avy-timeout-seconds 0.2)
  (global-set-key (kbd "M-g M-t") 'avy-goto-char-timer))

(use-package flycheck
  :config
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (bind-keys
               :map flycheck-mode-map
               ("M-g M-e" . avy-flycheck-goto-error)))))

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

(use-package clj-refactor
  :defer t
  :hook clojure-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-favor-prefix-notation nil))

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
  :defer t
  :pin melpa-stable
  :config
  (add-hook 'slime-mode-hook 'smartparens-mode)
  (add-hook 'slime-repl-mode-hook 'smartparens-mode)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy)))

(use-package ac-slime
  :defer t
  :pin melpa-stable
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(use-package cc-mode
  :defer t
  :init (add-auto-mode 'c++-mode "\\.ino$")
  :hook ((c-mode . auto-complete-mode)
         (c++-mode . auto-complete-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(use-package elpy
  :defer t
  :config
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules))
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python")

  :bind (:map elpy-mode-map
              ("C-c z" . elpy-shell-switch-to-shell)
              ("M-g M-n" . elpy-flymake-next-error)
              ("M-g M-p" . elpy-flymake-previous-error)
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("C-c C-p" . nil)
              ("C-c C-n" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)))

;; (eval-after-load "elpy"
;;   '(cl-dolist (key '("M-<left>" "M-<right>" "C-c C-p" "C-c C-n"))
;;      (define-key elpy-mode-map (kbd key) nil)))

(setq ian/python-cols 88)

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

;; (defvar pyenv-current-version nil nil)

;; (defun pyenv-init()
;;   "Initialize pyenv's current version to the global one."
;;   (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
;;     (message (concat "Setting pyenv version to " global-pyenv))
;;     (pyenv-mode-set global-pyenv)
;;     (setq pyenv-current-version global-pyenv)))

;; (add-hook 'after-init-hook 'pyenv-init)


(add-hook 'python-mode-hook
          (lambda ()
            ;; Set up fill column indicator
            (set-fill-column ian/python-cols)
            (fci-mode)
            (setq fci-rule-column ian/python-cols
                  show-trailing-whitespace t)

            (flycheck-mode)

            (setq python-shell-completion-native-enable nil)
            (pyenv-mode)

            (elpy-mode)
            (elpy-enable)

            (bind-keys :map python-mode-map
                       ("C-M-f" . python-nav-forward-sexp-safe)
                       ("C-M-b" . python-nav-backward-sexp-safe))))

(use-package highlight-indent-guides
  :config
  (progn
    ;;(setq highlight-indent-guides-method 'column)
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-auto-odd-face-perc 3)
    (setq highlight-indent-guides-auto-even-face-perc 7)
    (setq highlight-indent-guides-auto-character-face-perc 10)
    (add-hook 'python-mode-hook 'highlight-indent-guides-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web stuff

(use-package web-mode
  :defer t
  :config
  (add-auto-mode 'web-mode
                 "*html*" "*twig*" "*tmpl*" "\\.erb" "\\.rhtml$" "\\.ejs$" "\\.hbs$"
                 "\\.ctp$" "\\.tpl$"
                 "/\\(views\\|html\\|templates\\)/.*\\.php$")
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; (use-package php-mode
;;   :config
;;   (add-hook 'php-mode-hook
;;             (lambda ()
;;               (setq c-basic-offset 2))))

(use-package puppet-mode
  :defer t
  :pin melpa-stable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust

(use-package flycheck-rust
  ;; :pin melpa-stable
  :defer t
  )

(use-package flycheck-inline
  ;; :pin melpa-stable
  :after flycheck
  :hook flycheck)

(use-package rust-mode
  ;; :pin melpa-stable
  :defer t
  :config
  (setq rust-format-on-save t)
  (with-eval-after-load 'rust-mode
    (add-hook 'rust-mode-hook #'flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cargo
  ;; :pin melpa-stable
  :defer t
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (setq compilation-ask-about-save nil))

(use-package racer
  ;; :pin melpa-stable
  :defer t
  :requires rust-mode
  :init
  (setq company-tooltip-align-annotations t)

  ;; Problems with racer having an incorrect RUST_SRC_PATH were fixed
  ;; with this setting, and by installing the rust sources with
  ;; `rustup component add rust-src`.
  :init (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/src"))

  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup and text formatting

(use-package markdown-mode
  :defer t
  :pin melpa-stable)

(use-package yaml-mode
  :defer t
  :pin melpa-stable)

(use-package highlight-indentation
  :defer t
  :config
  (add-hook 'web-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'web-mode-hook #'highlight-indentation-current-column-mode)

  (add-hook 'html-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'html-mode-hook #'highlight-indentation-current-column-mode)

  (add-hook 'yaml-mode-hook #'highlight-indentation-current-column-mode))

(use-package graphviz-dot-mode
  :defer t)

(use-package deft
  :defer t
  :pin melpa-stable
  :config
  (setq deft-directory "~/org"
        deft-recursive t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(use-package es-mode
  :defer t)

(use-package ob-http
  :defer t)

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (load-user-file "modes/org.el"))

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
