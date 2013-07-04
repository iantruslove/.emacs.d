(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-theme 'misterioso t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(cl
                      nrepl
                      clojure-mode
                      clojure-test-mode
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      markdown-mode
                      yasnippet
                      yas-jit
                      zencoding-mode
                      ;emacs-pry
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Load window management crap
;(load "~/.emacs.d/emacsd-tile.el")

;; ClojureScript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; Markdown support
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

(electric-indent-mode +1)

;; Enable winner mode - C-c <left> to go back a window configuration
(winner-mode)

;; Enable rainbow parens for all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


