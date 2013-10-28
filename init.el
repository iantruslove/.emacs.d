;; Initialize package management with Cask and Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Load *.el from the configs directory
(defvar it/config-dir "~/.emacs.d/custom")
(add-to-list 'load-path it/config-dir)
(dolist (config-file (directory-files it/config-dir nil ".*el$"))
  (load config-file))
