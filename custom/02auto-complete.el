(require 'auto-complete-config)

(ac-config-default)
(setq ac-ignore-case nil)

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
				c-mode cc-mode c++-mode
				clojure-mode java-mode
				perl-mode cperl-mode python-mode ruby-mode
				ecmascript-mode javascript-mode js2-mode php-mode css-mode
				makefile-mode sh-mode fortran-mode f90-mode ada-mode
				xml-mode sgml-mode
				haskell-mode literate-haskell-mode
				emms-tag-editor-mode
				asm-mode
				org-mode)
	      (add-to-list 'ac-modes mode)))
