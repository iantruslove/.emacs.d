;;; clojure-test-toggle.el --- for toggling between Clojure code and tests

;;; Commentary:

;; Totally stolen from the deprecated sections of clojure-test-mode,
;; which is originally
;; Copyright (c) 2011 Phil Hagelberg

;;; Code:

(require 'clojure-mode)

(defun clojure-underscores-for-hyphens (namespace)
  "Replace all hyphens in NAMESPACE with underscores."
  (replace-regexp-in-string "-" "_" namespace))

(defun clojure-test-for (namespace)
  "Return the path of the test file for the given NAMESPACE."
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\.")))
    (format "%stest/%s_test.clj"
            (file-name-as-directory
             (locate-dominating-file buffer-file-name "src/"))
            (mapconcat 'identity segments "/"))))

(defun clojure-test-implementation-for (namespace)
  "Returns the path of the src file for the given test namespace."
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (namespace-end (split-string (car (last segments)) "_"))
         (namespace-end (mapconcat 'identity (butlast namespace-end) "_"))
         (impl-segments (append (butlast segments) (list namespace-end))))
    (format "%s/src/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity impl-segments "/"))))

(defvar clojure-test-for-fn 'clojure-test-for
  "The function that will return the full path of the Clojure test file for the given namespace.")

(defvar clojure-test-implementation-for-fn 'clojure-test-implementation-for
  "Var pointing to the function that will return the full path of the
Clojure src file for the given test namespace.")

(defun clojure-in-tests-p ()
  "Check whether the current file is a test file.

Two checks are made - whether the namespace of the file has the
word test in it and whether the file lives under the test/ directory."
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun clojure-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (funcall clojure-test-for-fn (clojure-find-ns))))

(defun clojure-test-jump-to-implementation ()
  "Jump from test file to implementation."
  (interactive)
  (find-file (funcall clojure-test-implementation-for-fn
                      (clojure-find-ns))))

(defun clojure-test-toggle ()
  "Jump between implementation and related test file."
  (interactive)
  (if (clojure-in-tests-p)
      (clojure-test-jump-to-implementation)
    (clojure-jump-to-test)))

(provide 'clojure-test-toggle)
;;; clojure-test-toggle ends here
