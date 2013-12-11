;; Fire up Paredit when in various clojure modes
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Enable eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Rainbow delimiters please
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Hide some of the cider mode special buffers
;;(setq nrepl-hide-special-buffers t)

;; Set up clj-refactor-mode
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1))) ;; until YAS is enabled throughout
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
			       (clj-refactor-mode 1)
			       (cljr-add-keybindings-with-prefix "C-c C-r")
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
