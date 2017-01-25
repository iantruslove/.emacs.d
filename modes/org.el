;; Org
(use-package org
  :ensure t
  :pin melpa)

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
      org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

(defvar organizer (concat org-directory "organizer.org"))
(defvar cambium-organizer (concat org-directory "cambium/cambium-organizer.org"))
(setq journal (concat org-directory "journal.org"))
(defvar lg-journal (concat org-directory "lg/lg-journal.org"))

(setq org-default-notes-file 'organizer)

;; Speed commands
(setq org-use-speed-commands t)
(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Quick shortcuts to open important files:
(global-set-key (kbd "C-c o")
                (lambda ()
                  (interactive)
                  (find-file organizer)))
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (find-file lg-journal)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(i!)" "WAIT(w@/!)"
                  "|"
                  "DONE(d!)" "CANCELED(c@)")))

(setq org-tag-alist '(;; GTD-ish Contexts:
                      (:startgroup . nil)
                      ("@lg"       . ?L)
                      ("@cambium"  . ?G)
                      ("@home"     . ?H)
                      ("@nerding"  . ?N)
                      ("@errands"  . ?E)
                      ("@coding"   . ?C)
                      ("@phone"    . ?P)
                      ("@reading"  . ?R)
                      (:endgroup   . nil)

                      ;; ;; Structural
                      ("project" . ?p)  ;; identifies a TODO item as a project
                      ("inbox"   . ?i)

                      ;; ;; Others:
                      ("blog"    . ?b)
                      ("idea"    . ?d)
                      ("meeting" . ?m)
                      ("retro"   . ?o)
                      ("REFILE"  . ?r)
                      ("sm"      . ?s)
                      ("talk"    . ?t)))

;; Project tags shouldn't automatically propagate down to subtasks
(setq org-tags-exclude-from-inheritance '("project" "inbox"))

;; Don't show days in rolled-up times
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Size: %^{effort|0:05|0:15|0:30|1:00|2:00|4:00}
:Captured: %U
:END:
%?

%i
" "Basic task data")

(setq org-capture-templates
      `(
        ;; ("T" "Detailed Task" entry
        ;;  (file+headline organizer "Inbox")
        ;;  ,my/org-basic-task-template)
        ("T" "Quick task" entry
         (file+headline organizer "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :immediate-finish t)

        ("t" "Quick LG journal task" entry
         (file+datetree lg-journal)
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ("c" "Quick Cambium TODO" entry
         (file+headline cambium-organizer "Cambium Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ("j" "Quick LG Journal Entry" entry
         (file+datetree lg-journal)
         "* %^{Title}\n:PROPERTIES:\n:Captured: [%<%H:%M>]\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ;; ("J" "Context-specific Journal Entry"
        ;;  entry (file+datetree journal)
        ;;  "* %?\n:PROPERTIES:\n:Captured:[%<%H:%M>]\n:END:\n\n  %i\n\n  From: %a"
        ;;  :empty-lines 1)
        ))

(setq org-agenda-custom-commands
      '(("A" "Agenda and todos"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-deadline-warning-days 7)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-agenda-repeating-timestamp-show-all nil)))

          (tags-todo "DEADLINE<\"<+0d>\""
                     ((org-agenda-overriding-header "OVERDUE")))
          (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                     ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
          (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                     ((org-agenda-overriding-header "HIGH PRIORITY")))
          (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                     ((org-agenda-overriding-header "FLAGGED")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-when-regexp-matches))
                      (org-agenda-skip-regexp "\\[#A\\]")))
          (tags-todo "@lg"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "cambium"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "@home")
          (tags-todo "@nerding")
          (tags-todo "@errands")
          (tags-todo "@coding")
          (tags-todo "@phone")
          (tags-todo "@reading")))

        ("L" "Long agenda"
         ((agenda ""
                  ((org-agenda-ndays 60)
                   (org-deadline-warning-days 7)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-agenda-time-grid nil)
                   (org-agenda-repeating-timestamp-show-all nil)
                   (org-agenda-show-log t)
                   ;; only show days with something:
                   (org-agenda-show-all-dates nil)))))

        ("K" "tasK summary"
         ((tags-todo "REFILE"
                     ((org-agenda-overriding-header "Tasks to refile")
                      (org-agenda-sorting-strategy '(priority-down))))
          (todo "IN PROGRESS"
                ((org-agenda-overriding-header "Tasks In Progress")
                 (org-agenda-sorting-strategy '(priority-down))))
          (todo "TODO"
                ((org-agenda-overriding-header "Tasks TODO")
                 (org-agenda-skip-function '(my/org-agenda-skip-with-match ":REFILE:"))
                 (org-agenda-sorting-strategy '(priority-down))))))

        ("D" "toDay"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-show-log t)
                   (org-agenda-time-grid nil)
                   (org-agenda-log-mode-items '(state))
                   (org-agenda-sorting-strategy
                    (quote ((agenda time-up priority-down tag-up))))
                   (org-deadline-warning-days 0)))
          (todo "IN PROGRESS"
                ((org-agenda-overriding-header "Tasks In Progress")
                 (org-agenda-sorting-strategy '(priority-down))))))

        ("td" "Daily Timesheet"
         ((agenda ""))
         ((org-agenda-log-mode-items '(clock closed))
          (org-agenda-overriding-header "DAILY TIMESHEET")
          (org-agenda-show-log 'clockcheck)
          (org-agenda-span 'day)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("tw" "Weekly Timesheet"
         ((agenda ""))
         (
          ;; (org-agenda-format-date "")
          (org-agenda-overriding-header "WEEKLY TIMESHEET")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
          (org-agenda-span 'week)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("tm" "Monthly Timesheet"
         ((agenda ""))
         (
          ;; (org-agenda-format-date "")
          (org-agenda-overriding-header "MONTHLY TIMESHEET")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
          (org-agenda-span 'month)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("S" "Scratch"
         ;;((tags "project+CATEGORY=\"elephants\""))
         ((tags "retro")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel and friends

(require 'ob-clojure)
(use-package es-mode
  :ensure t)

(use-package ob-http
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)
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
