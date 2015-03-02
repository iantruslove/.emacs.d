(eval-when-compile (require 'cl))

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

;; Place downloaded elisp files in ~/.emacs.d/vendor, or in subdirectories therein. 

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "vendor/" user-emacs-directory))
