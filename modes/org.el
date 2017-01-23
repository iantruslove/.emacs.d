;; Org
(use-package org
  :ensure t
  :pin melpa
  )

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'org-store-link)
            (local-set-key (kbd "C-c C-l") 'org-insert-link)
            ;; Always use org-indent-mode
            (org-indent-mode t)))

(setq org-directory "~/org/"
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-agenda-files '("~/org/")
      org-log-into-drawer 't
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      )
;;(setq org-completion-use-ido 't)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defvar organizer (concat org-directory "organizer.org"))
(defvar cambium-organizer (concat org-directory "cambium/cambium-organizer.org"))
(defvar journal (concat org-directory "journal.org"))
(defvar lg-journal (concat org-directory "lg/lg-journal.org"))

(setq org-default-notes-file 'organizer)

;; Speed commands
(setq org-use-speed-commands t)
(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel and friends

(require 'ob-clojure)
(use-package es-mode
  :ensure t)

(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (sh . t)
     (elasticsearch . t)))

(setq org-babel-sh-command "bash"
        org-babel-clojure-backend 'cider
        org-confirm-babel-evaluate nil
        org-babel-default-header-args:clojure '((:results . "value verbatim pp"))
        org-babel-default-header-args:sh '((:results . "verbatim drawer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic block encryption: encrypt any heading contents tagged with ":crypt:".

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)
