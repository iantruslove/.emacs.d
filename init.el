;; Initialize package management with Cask and Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/custom")

(load "00common-setup.el")
(load "01colors.el")
(load "10projectile.el")
(load "20javascript.el")

