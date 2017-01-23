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

(use-package auto-compile
  :ensure t
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(setq inhibit-splash-screen t
      make-backup-files nil
      column-number-mode t
      x-select-enable-clipboard t ;; makes killing/yanking interact with the clipboard
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package fill-column-indicator
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$" ".*png$" ".*cache$")
        recentf-max-saved-items 60))

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  ;;(global-set-key (kbd "C-c C-r") 'ivy-resume) ;; interesting. TODO put it into a hydra
  (setq ivy-use-virtual-buffers t
        ;;ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)) ;; This is really slow on big lists.
        ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-M-s") 'swiper))

(use-package hydra
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  (setq-default magit-process-popup-time 10
                magit-diff-refine-hunk nil
                magit-auto-revert-mode t))

(use-package gist
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes and appearance

(use-package color-theme-sanityinc-tomorrow
  :ensure t
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
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package aggressive-indent
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  ;; load up smartparens default config - https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el
  (require 'smartparens-config)

  (bind-keys
   :map smartparens-mode-map
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

   ;; ("C-<right>" . sp-forward-slurp-sexp)
   ;; ("M-<right>" . sp-forward-barf-sexp)
   ;; ("C-<left>"  . sp-backward-slurp-sexp)
   ;; ("M-<left>"  . sp-backward-barf-sexp)

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

(use-package eval-sexp-fu
  :ensure t)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
(put 'use-package 'lisp-indent-function 1)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (smartparens-mode)
                                  (aggressive-indent-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(setq ian/clojure-cols 80)

(use-package clojure-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.clj.*$" . clojure-mode))
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; This is useful for working with camel-case tokens, like names of
              ;; Java classes (e.g. JavaClassName)
              (subword-mode)
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
;;(require 'clojure-mode-extra-font-locking)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (setq
   ;; go right to the REPL buffer when it's finished connecting
   cider-repl-pop-to-buffer-on-connect t

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
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom modules
(load-user-file "modes/org.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Site-specific overrides
(load-all-in-directory "site")


;; DONE structural editing for lisps
;; TODO Figure out structural editing keystrokes - kill sexps/all sexps, slurpage, ...
;; TODO org mode
;; DONE highlight evaluated regions
;; IN PROGRESS automatic byte compile
;; - is this quite working properly? The "site" directory stuff isn't yet...
;; TODO org-todos
;; TODO spacemacs multiple pane buffer wrapping
;; DONE continual scrolling. Spacemacs has a nice one.
;; TODO: set up some hydras
;; - clj-refactor
;; - cider
;; - smartparens
;; - window movements and rearrangements
;; TODO window operations - vim-like would be nice for moving
;; TODO font - monaco 13, iirc
;; DONE Don't keep <foo>~ backup files
