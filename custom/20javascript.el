;; Use js2-mode instead of javascript-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

;; Enable js2-refactor
(js2r-add-keybindings-with-prefix "C-c C-m")
