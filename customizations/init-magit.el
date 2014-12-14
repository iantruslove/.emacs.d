(require 'magit)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk nil
 magit-completing-read-function 'magit-ido-completing-read)

(global-set-key (kbd "C-c g") 'magit-status)

;;; gist mode
(require 'gist)
