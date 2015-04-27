(require 'magit)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk nil
 magit-completing-read-function 'magit-ido-completing-read
 magit-auto-revert-mode t
 magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-c g") 'magit-status)

;;; gist mode
(require 'gist)
