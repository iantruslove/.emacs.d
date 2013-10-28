;; Use js2-mode instead of javascript-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Enable js2-refactor
(js2r-add-keybindings-with-prefix "C-c C-m")


